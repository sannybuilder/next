use crate::{
    op::*,
    parser::{
        node_span_with, ArgType, AstNode, AstNodeSpan, NodeCallExpression, NodeIndexExpression,
    },
    scope::{Function, ScopeType, Scopes, VarType},
    tokens::Token,
};
use anyhow::{anyhow, bail, ensure, Ok, Result};

#[derive(Debug, Clone)]
pub enum Instruction {
    OpcodeInst(OpcodeInst),
    VarDecl(VarDecl),
    ArrayDecl(ArrayDecl),
    Label(String),
    RawBytes(Vec<u8>),
}

macro_rules! NOT {
    ($id: expr) => {
        $id + 0x8000
    };
}

macro_rules! op {
    ($id: expr => $( $arg:expr ),*) => {
        Instruction::OpcodeInst(OpcodeInst {
            id: $id,
            args: vec![$($arg)*],
            is_variadic: false,
        })
    };
}

macro_rules! label {
    ($name: expr) => {
        Instruction::Label($name.to_string())
    };
}

#[derive(Debug, Clone)]
pub struct OpcodeInst {
    pub id: u16,
    pub args: Vec<OpcodeArgument>,
    pub is_variadic: bool,
}

type TVarIndex = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum OpcodeArgument {
    INT(i32),
    INT32(i32),
    FLOAT(f32),
    LABEL(String),
    GVAR(TVarIndex, ArgType),
    LVAR(TVarIndex, ArgType),
    STR(Vec<u8>),
    ARRAY(Box<(OpcodeArgument, OpcodeArgument, usize)>), // var[index]
}

impl From<i32> for OpcodeArgument {
    fn from(value: i32) -> Self {
        OpcodeArgument::INT(value)
    }
}

impl From<usize> for OpcodeArgument {
    fn from(value: usize) -> Self {
        OpcodeArgument::INT(value as i32)
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub ty: ArgType,
}

#[derive(Debug, Clone)]
pub struct ArrayDecl {
    pub name: String,
    pub ty: ArgType,
    pub count: i32,
}

pub fn compile(program: AstNodeSpan) -> Result<Vec<Instruction>> {
    let mut scopes = Scopes::new();
    let mut instructions = vec![];

    scopes.enter(ScopeType::Root, 0);
    visit_stmt(
        &program,
        &mut |inst| {
            instructions.push(inst);
        },
        &mut scopes,
    )?;
    scopes.exit();
    ensure!(scopes.is_empty(), "Unclosed scope. Missing 'end'.");

    fn prepend_vec(vec: &mut Vec<Instruction>, mut prepend: Vec<Instruction>) {
        for inst in prepend.drain(..).rev() {
            vec.insert(0, inst);
        }
    }

    if !scopes.get_exported_functions().is_empty() {
        let mut buf = vec![];

        // magic
        buf.extend(&[0xFF, 0x7F, 0xFE, 0x00, 0x00]);
        // EXPT
        buf.extend(&[0x45, 0x58, 0x50, 0x54]);
        // export size
        let size = scopes.calculate_export_section_size();
        buf.extend(&size.to_le_bytes());

        for func in scopes.get_exported_functions() {
            buf.extend(func.name.as_bytes());
            buf.push(0);

            //insert 4 bytes for function address/offset inserted by codegen
            buf.extend(&[0, 0, 0, 0]);

            buf.push(func.definition.input_count as u8);
            for i in 0..func.definition.input_count {
                let ty = func.definition.input[i];
                buf.push(ty.into());
            }

            buf.push(func.definition.output_count as u8);
            for i in 0..func.definition.output_count {
                let ty = func.definition.output[i];
                buf.push(ty.into());
            }

            let mut flags = 0;

            if func.definition.is_logical {
                flags |= 1 << 0;
            }

            if func.definition.is_optional {
                flags |= 1 << 1;
            }

            if true
            /*func.definition.is_static*/
            {
                flags |= 1 << 2; // local functions are always static. change when foreign functions are supported
            }

            // if func.definition.has_variadic_input {
            //     buf.push(flags | 1 << 3);
            // }

            // if func.definition.has_variadic_output {
            //     buf.push(flags | 1 << 4);
            // }

            buf.push(flags);
            buf.extend(&[0, 0, 0, 0]); //unused
        }

        let exit_label = scopes.unique_label();

        prepend_vec(
            &mut instructions,
            vec![
                Instruction::OpcodeInst(OpcodeInst {
                    id: OP_JUMP,
                    args: vec![OpcodeArgument::LABEL(exit_label.clone())],
                    is_variadic: false,
                }),
                Instruction::RawBytes(buf),
                Instruction::Label(exit_label),
            ],
        );
    }

    if scopes.get_heap_size() > 0 {
        prepend_vec(
            &mut instructions,
            vec![
                Instruction::OpcodeInst(OpcodeInst {
                    id: OP_ALLOCATE_MEMORY,
                    args: vec![
                        OpcodeArgument::INT(scopes.get_heap_size() as _),
                        OpcodeArgument::LVAR(0, ArgType::Int),
                    ],
                    is_variadic: false,
                }),
                Instruction::OpcodeInst(OpcodeInst {
                    id: OP_GET_VAR_POINTER,
                    args: vec![
                        OpcodeArgument::GVAR(0, ArgType::Int),
                        OpcodeArgument::LVAR(33, ArgType::Int),
                    ],
                    is_variadic: false,
                }),
                Instruction::OpcodeInst(OpcodeInst {
                    id: 0x0062,
                    args: vec![
                        OpcodeArgument::LVAR(0, ArgType::Int),
                        OpcodeArgument::LVAR(33, ArgType::Int),
                    ],
                    is_variadic: false,
                }),
                Instruction::OpcodeInst(OpcodeInst {
                    id: 0x0016,
                    args: vec![
                        OpcodeArgument::LVAR(0, ArgType::Int),
                        OpcodeArgument::INT(4),
                    ],
                    is_variadic: false,
                }),
            ],
        );
    }

    return Ok(instructions);
}

fn visit_exprs<'a, Emitter>(
    this_node: &'a AstNodeSpan,
    emit: &mut Emitter,
    scopes: &mut Scopes,
) -> Result<Vec<(&'a AstNodeSpan, OpcodeArgument, ArgType)>>
where
    Emitter: FnMut(Instruction) -> (),
{
    let mut exprs = vec![];

    match &this_node.node {
        AstNode::NodeList(inner_node) => {
            for expr in inner_node {
                exprs.extend(visit_exprs(expr, emit, scopes)?);
            }
        }
        AstNode::NodeCallExpression(inner_node) => {
            let (results, _) = visit_call_expr(
                &inner_node,
                emit,
                scopes,
                &mut default_allocator,
                this_node,
                false,
            )?;
            if results.is_empty() {
                bail!(
                    "Function '{}' does not return anything at line {}",
                    inner_node.name,
                    inner_node.name.line
                );
            }
            exprs.extend(results.iter().map(|r| (this_node, r.0.clone(), r.1)));
        }
        _other_node => {
            let (arg, ty) = visit_expr(this_node, emit, scopes, &mut default_allocator)?;
            exprs.push((this_node, arg, ty))
        }
    }

    Ok(exprs)
}

fn visit_assignment_exprs<'a, Emitter, Allocator>(
    this_node: &'a AstNodeSpan,
    emit: &mut Emitter,
    scopes: &mut Scopes,
    allocate: &mut Allocator,
) -> Result<()>
where
    Emitter: FnMut(Instruction) -> (),
    Allocator: FnMut(&AstNodeSpan, ArgType, &mut Emitter, &mut Scopes) -> Result<OpcodeArgument>,
{
    match &this_node.node {
        AstNode::NodeList(inner_node) => {
            for expr in inner_node {
                visit_assignment_exprs(expr, emit, scopes, allocate)?;
            }
        }
        AstNode::NodeCallExpression(inner_node) => {
            let (results, _) =
                visit_call_expr(&inner_node, emit, scopes, allocate, this_node, false)?;
            if results.is_empty() {
                bail!(
                    "Function '{}' does not return anything at line {}",
                    inner_node.name,
                    inner_node.name.line
                );
            }
        }
        AstNode::NodeBinary(_) | AstNode::NodeUnary(_) => {
            let (arg, ty) = visit_expr(this_node, emit, scopes, allocate)?;

            use core::result::Result::Ok;

            match arg {
                OpcodeArgument::INT(_) | OpcodeArgument::FLOAT(_) => {
                    match allocate(this_node, ty, emit, scopes) {
                        Ok(target) => {
                            let inst = Instruction::OpcodeInst(OpcodeInst {
                                id: get_assignment_opcode(&ty, this_node)?,
                                args: vec![target, arg],
                                is_variadic: false,
                            });
                            emit(inst);
                        }
                        // don't emit assignment if allocate handled it (in case of pointers)
                        Err(e) if e.to_string() == "skip" => {}
                        Err(e) => bail!(e),
                    }
                }
                _ => {
                    // assignment done in visit_expr already
                }
            }
        }
        _other_node => {
            let (arg, ty) = visit_expr(this_node, emit, scopes, allocate)?;
            use core::result::Result::Ok;
            match allocate(this_node, ty, emit, scopes) {
                Ok(target) => {
                    let inst = Instruction::OpcodeInst(OpcodeInst {
                        id: get_assignment_opcode(&ty, this_node)?,
                        args: vec![target, arg],
                        is_variadic: false,
                    });
                    emit(inst);
                }
                // don't emit assignment if allocate handled it (in case of pointers)
                Err(e) if e.to_string() == "skip" => {}
                Err(e) => bail!(e),
            }
        }
    }

    Ok(())
}

fn visit_const_exprs<'a>(
    this_node: &'a AstNodeSpan,
    scopes: &mut Scopes,
) -> Result<Vec<(&'a AstNodeSpan, OpcodeArgument, ArgType)>> {
    let mut exprs = vec![];

    match &this_node.node {
        AstNode::NodeList(inner_node) => {
            for expr in inner_node {
                exprs.extend(visit_const_exprs(expr, scopes)?);
            }
        }
        _other_node => {
            let (arg, ty) = visit_const_expr(this_node, scopes)?;
            exprs.push((this_node, arg, ty))
        }
    }
    Ok(exprs)
}

fn visit_const_expr(
    this_node: &AstNodeSpan,
    scopes: &mut Scopes,
) -> Result<(OpcodeArgument, ArgType)> {
    let mut pure = true;
    let (arg, ty) = visit_expr(
        this_node,
        &mut |_| {
            pure = false;
        },
        scopes,
        &mut default_allocator,
    )?;
    if !pure {
        bail!(
            "Can't evaluate const expression {} at line {}",
            this_node,
            this_node.line
        );
    }
    Ok((arg, ty))
}

fn visit_expr<Emitter, Allocator>(
    this_node: &AstNodeSpan,
    emit: &mut Emitter,
    scopes: &mut Scopes,
    allocate: &mut Allocator,
) -> Result<(OpcodeArgument, ArgType)>
where
    Emitter: FnMut(Instruction) -> (),
    Allocator: FnMut(&AstNodeSpan, ArgType, &mut Emitter, &mut Scopes) -> Result<OpcodeArgument>,
{
    let arg = match &this_node.node {
        AstNode::NodeIntLiteral(_)
        | AstNode::NodeFloatLiteral(_)
        | AstNode::NodeLabelLiteral(_)
        | AstNode::NodeIdentifier(_)
        | AstNode::NodeStringLiteral(_) => {
            let arg = node_to_argument(this_node, scopes)?;
            let ty = get_node_type(this_node, scopes)?;
            (arg, ty)
        }
        AstNode::NodeBinary(node) => match &node.op {
            Token::And => {
                let pick_left_label = scopes.unique_label();
                let push_exit_label = scopes.unique_label();
                let (lhs, ty1) = visit_expr(&node.lhs, emit, scopes, &mut default_allocator)?;
                deallocate_temp_var(&lhs, scopes)?;
                emit(op!(OP_IS_TRUTHY => lhs.clone()));
                emit(op!(OP_JF => OpcodeArgument::LABEL(pick_left_label.clone())));
                let (rhs, ty2) = visit_expr(&node.rhs, emit, scopes, &mut default_allocator)?;
                deallocate_temp_var(&rhs, scopes)?;
                ensure!(
                    match_types(ty1, ty2),
                    "Incompatible types: {ty1}({}) and {ty2}({}) at line {}",
                    &node.lhs,
                    &node.rhs,
                    this_node.line
                );
                let target = allocate(this_node, ty1, emit, scopes)?;
                if target != rhs {
                    emit(Instruction::OpcodeInst(OpcodeInst {
                        id: get_assignment_opcode(&ty2, &node.rhs)?,
                        args: vec![target.clone(), rhs],
                        is_variadic: false,
                    }));
                }
                if target != lhs {
                    emit(op!(OP_JUMP => OpcodeArgument::LABEL(push_exit_label.clone())));
                }
                emit(label!(pick_left_label));
                if target != lhs {
                    emit(Instruction::OpcodeInst(OpcodeInst {
                        id: get_assignment_opcode(&ty1, &node.lhs)?,
                        args: vec![target.clone(), lhs],
                        is_variadic: false,
                    }));
                }
                emit(label!(push_exit_label));
                (target, ty1)
            }
            Token::Or => {
                let check_right_label = scopes.unique_label();
                let push_exit_label = scopes.unique_label();
                let (lhs, ty1) = visit_expr(&node.lhs, emit, scopes, &mut default_allocator)?;
                deallocate_temp_var(&lhs, scopes)?;
                emit(op!(OP_IS_TRUTHY => lhs.clone()));
                emit(op!(OP_JF => OpcodeArgument::LABEL(check_right_label.clone())));
                let target = allocate(this_node, ArgType::Int, emit, scopes)?;
                if target != lhs {
                    emit(Instruction::OpcodeInst(OpcodeInst {
                        id: get_assignment_opcode(&ty1, &node.lhs)?,
                        args: vec![target.clone(), lhs],
                        is_variadic: false,
                    }));
                }
                emit(op!(OP_JUMP => OpcodeArgument::LABEL(push_exit_label.clone())));
                emit(label!(check_right_label));
                let (rhs, ty2) = visit_expr(&node.rhs, emit, scopes, &mut default_allocator)?;
                deallocate_temp_var(&rhs, scopes)?;
                ensure!(
                    match_types(ty1, ty2),
                    "Incompatible types: {ty1}({}) and {ty2}({}) at line {}",
                    &node.lhs,
                    &node.rhs,
                    this_node.line
                );
                if target != rhs {
                    emit(Instruction::OpcodeInst(OpcodeInst {
                        id: get_assignment_opcode(&ty2, &node.rhs)?,
                        args: vec![target.clone(), rhs],
                        is_variadic: false,
                    }));
                }
                emit(label!(push_exit_label));
                (target, ty1)
            }
            Token::Less
            | Token::LessEqual
            | Token::Greater
            | Token::GreaterEqual
            | Token::EqualEqual
            | Token::NotEqual => {
                bail!(
                    "Unexpected logical operator {} in expression at line {}",
                    node.op,
                    this_node.line
                );
            }
            _ => {
                let (left, ty1) = visit_expr(&node.lhs, emit, scopes, &mut default_allocator)?;
                let (mut right, ty2) = visit_expr(&node.rhs, emit, scopes, &mut default_allocator)?;
                ensure!(
                    match_types(ty1, ty2),
                    "Incompatible types: {ty1}({}) and {ty2}({}) at line {}",
                    &node.lhs,
                    &node.rhs,
                    this_node.line
                );

                // const eval
                match node.op {
                    Token::Add => match (&left, &right) {
                        (OpcodeArgument::INT(l), OpcodeArgument::INT(r)) => {
                            return Ok((OpcodeArgument::INT(l + r), ArgType::Int))
                        }
                        (OpcodeArgument::FLOAT(l), OpcodeArgument::FLOAT(r)) => {
                            return Ok((OpcodeArgument::FLOAT(l + r), ArgType::Float))
                        }
                        _ => {}
                    },
                    Token::Sub => match (&left, &right) {
                        (OpcodeArgument::INT(l), OpcodeArgument::INT(r)) => {
                            return Ok((OpcodeArgument::INT(l - r), ArgType::Int))
                        }
                        (OpcodeArgument::FLOAT(l), OpcodeArgument::FLOAT(r)) => {
                            return Ok((OpcodeArgument::FLOAT(l - r), ArgType::Float))
                        }
                        _ => {}
                    },
                    Token::Mul => match (&left, &right) {
                        (OpcodeArgument::INT(l), OpcodeArgument::INT(r)) => {
                            return Ok((OpcodeArgument::INT(l * r), ArgType::Int))
                        }
                        (OpcodeArgument::FLOAT(l), OpcodeArgument::FLOAT(r)) => {
                            return Ok((OpcodeArgument::FLOAT(l * r), ArgType::Float))
                        }
                        _ => {}
                    },
                    Token::Div => match (&left, &right) {
                        (OpcodeArgument::INT(l), OpcodeArgument::INT(r)) => {
                            if l % r == 0 {
                                return Ok((OpcodeArgument::INT(l / r), ArgType::Int));
                            }
                        }
                        (OpcodeArgument::FLOAT(l), OpcodeArgument::FLOAT(r)) => {
                            return Ok((OpcodeArgument::FLOAT(l / r), ArgType::Float))
                        }
                        _ => {}
                    },
                    Token::BitAnd => match (&left, &right) {
                        (OpcodeArgument::INT(l), OpcodeArgument::INT(r)) => {
                            return Ok((OpcodeArgument::INT(l & r), ArgType::Int))
                        }
                        _ => {}
                    },
                    Token::BitOr => match (&left, &right) {
                        (OpcodeArgument::INT(l), OpcodeArgument::INT(r)) => {
                            return Ok((OpcodeArgument::INT(l | r), ArgType::Int))
                        }
                        _ => {}
                    },
                    Token::BitXor => match (&left, &right) {
                        (OpcodeArgument::INT(l), OpcodeArgument::INT(r)) => {
                            return Ok((OpcodeArgument::INT(l ^ r), ArgType::Int))
                        }
                        _ => {}
                    },
                    _ => {}
                };

                let target = allocate(this_node, ty1, emit, scopes)?;
                deallocate_temp_var(&left, scopes)?;
                deallocate_temp_var(&right, scopes)?;

                match ty1 {
                    ArgType::Float => {
                        // this can be removed when FLOAT_* opcodes are added in CLEO

                        if target != left {
                            if target == right {
                                let temp = default_allocator(this_node, ty1, emit, scopes)?;
                                emit(Instruction::OpcodeInst(OpcodeInst {
                                    id: get_assignment_opcode(&ty1, &node.rhs)?,
                                    args: vec![temp.clone(), right.clone()],
                                    is_variadic: false,
                                }));

                                right = temp;
                            }
                            emit(Instruction::OpcodeInst(OpcodeInst {
                                id: get_assignment_opcode(&ty1, &node.lhs)?,
                                args: vec![target.clone(), left],
                                is_variadic: false,
                            }));
                        }

                        let opcode = match &node.op {
                            Token::Add => ADD_VAL_TO_FLOAT_LVAR,
                            Token::Sub => SUB_VAL_FROM_FLOAT_LVAR,
                            Token::Mul => MULT_FLOAT_LVAR_BY_VAL,
                            Token::Div => DIV_FLOAT_LVAR_BY_VAL,
                            _ => {
                                bail!("Invalid operator {} at line {}", &node.op, this_node.line);
                            }
                        };

                        emit(Instruction::OpcodeInst(OpcodeInst {
                            id: opcode,
                            args: vec![target.clone(), right],
                            is_variadic: false,
                        }));
                    }
                    _ => {
                        // note: if left operand matches target, this can be shorten to compound assignment (similar to floats)
                        let inst = Instruction::OpcodeInst(OpcodeInst {
                            id: get_assignment_opcode_binary(&ty1, &node.op)
                                .map_err(|e| anyhow!("{e} at line {}", this_node.line))?,
                            args: vec![left, right, target.clone()],
                            is_variadic: false,
                        });
                        emit(inst);
                    }
                }
                (target, ty1)
            }
        },
        AstNode::NodeUnary(inner_node) => match inner_node.op {
            Token::BitNot => {
                let (arg, ty) = visit_expr(&inner_node.node, emit, scopes, &mut default_allocator)?;
                ensure!(
                    ty == ArgType::Int,
                    "Expected integer type, got {ty} at line {}",
                    this_node.line
                );
                // const eval
                if let OpcodeArgument::INT(i) = arg {
                    return Ok((OpcodeArgument::INT(!i), ArgType::Int));
                }
                let target = allocate(this_node, ty, emit, scopes)?;
                let inst = Instruction::OpcodeInst(OpcodeInst {
                    id: OP_BIT_NOT,
                    args: vec![target.clone(), arg],
                    is_variadic: false,
                });
                emit(inst);
                (target, ty)
            }
            Token::Not => {
                bail!(
                    "Unexpected logical operator {} in expression at line {}",
                    Token::Not,
                    this_node.line
                );
            }
            ref unknown_op => bail!("Unknown unary operator {unknown_op}"),
        },
        AstNode::NodeCallExpression(inner_node) => {
            let (args, _) = visit_call_expr(
                inner_node,
                emit,
                scopes,
                &mut default_allocator,
                this_node,
                false,
            )?;
            ensure!(
                args.len() == 1,
                "A function should have 1 output param to be used in an expression. '{}' has {} at line {}",
                inner_node.name,
                args.len(),
                inner_node.name.line
            );
            args[0].clone()
        }
        AstNode::NodeIndexExpression(inner_node) => visit_index_expr(inner_node, emit, scopes)?,
        other_node => bail!(
            "Expected expression, found {other_node} at line {}",
            this_node.line
        ),
    };

    Ok(arg)
}

fn visit_logical_expr<Emitter>(
    this_node: &AstNodeSpan,
    emit: &mut Emitter,
    scopes: &mut Scopes,
    negate_op: bool,
) -> Result<()>
where
    Emitter: FnMut(Instruction) -> (),
{
    match &this_node.node {
        AstNode::NodeCallExpression(inner_node) => {
            let (args, sig) = visit_call_expr(
                inner_node,
                emit,
                scopes,
                &mut default_allocator,
                this_node,
                negate_op,
            )?;
            ensure!(
                sig.is_logical,
                "Expected logical expression, found {inner_node} at line {}",
                this_node.line
            );
            ensure!(
                args.is_empty(),
                "Unexpected value returned from logical expression at line {}",
                this_node.line
            );
        }
        AstNode::NodeBinary(node)
            if (node.op == Token::And && !negate_op) || (node.op == Token::Or && negate_op) =>
        {
            let exit_label = scopes.unique_label();
            visit_logical_expr(&node.lhs, emit, scopes, negate_op)?;
            emit(op!(OP_JF => OpcodeArgument::LABEL(exit_label.clone())));
            visit_logical_expr(&node.rhs, emit, scopes, negate_op)?;
            emit(label!(exit_label));
        }
        AstNode::NodeBinary(node)
            if (node.op == Token::Or && !negate_op) || (node.op == Token::And && negate_op) =>
        {
            let next_label = scopes.unique_label();
            let exit_label = scopes.unique_label();
            visit_logical_expr(&node.lhs, emit, scopes, negate_op)?;
            emit(op!(OP_JF => OpcodeArgument::LABEL(next_label.clone())));
            emit(op!(OP_JUMP => OpcodeArgument::LABEL(exit_label.clone())));
            emit(label!(next_label));
            visit_logical_expr(&node.rhs, emit, scopes, negate_op)?;
            emit(label!(exit_label));
        }
        AstNode::NodeUnary(inner_node) if inner_node.op == Token::Not => {
            visit_logical_expr(&inner_node.node, emit, scopes, !negate_op)?;
        }
        AstNode::NodeBinary(node)
            if matches!(
                node.op,
                Token::EqualEqual
                    | Token::NotEqual
                    | Token::Less
                    | Token::LessEqual
                    | Token::Greater
                    | Token::GreaterEqual
            ) =>
        {
            let (lhs, ty1) = visit_expr(&node.lhs, emit, scopes, &mut default_allocator)?;
            let (rhs, ty2) = visit_expr(&node.rhs, emit, scopes, &mut default_allocator)?;
            ensure!(
                match_types(ty1, ty2),
                "Incompatible types: {ty1}({}) and {ty2}({}) at line {}",
                &node.lhs,
                &node.rhs,
                this_node.line
            );

            deallocate_temp_var(&lhs, scopes)?;
            deallocate_temp_var(&rhs, scopes)?;

            let op = get_logical_opcode(&ty1, &node.op)?;
            let inst = Instruction::OpcodeInst(OpcodeInst {
                id: if negate_op { NOT!(op) } else { op },
                args: vec![lhs, rhs],
                is_variadic: false,
            });
            emit(inst);
        }
        _ => {
            let (arg, _ty) = visit_expr(this_node, emit, scopes, &mut default_allocator)?;
            emit(op!(if negate_op { NOT!(OP_IS_TRUTHY) } else { OP_IS_TRUTHY } => arg));
        }
    };
    Ok(())
}

fn visit_stmt<Emitter>(
    this_node: &AstNodeSpan,
    emit: &mut Emitter,
    scopes: &mut Scopes,
) -> Result<()>
where
    Emitter: FnMut(Instruction) -> (),
{
    match &this_node.node {
        AstNode::Root(block) => {
            let scope = create_scope(&block, scopes)?;
            for stmt in scope {
                visit_stmt(stmt, emit, scopes)?;
            }
        }
        AstNode::NodeFuncDeclStml(inner_node) => {
            emit(Instruction::Label(inner_node.name.clone()));

            let ty = if inner_node.is_exported {
                ScopeType::ExportedFunction
            } else {
                ScopeType::InternalFunction
            };
            scopes.enter(ty(inner_node.name.clone()), this_node.line);

            let scope = create_scope(&inner_node.body, scopes)?;

            // register function arguments in current scope
            for (arg_name, arg_ty) in inner_node.args.iter() {
                match &arg_ty.node {
                    AstNode::NodeIdentifier(ty) => {
                        scopes
                            .register_var(arg_name.clone(), ty.into(), VarType::Local)
                            .map_err(|e| anyhow!("{e} at line {}", this_node.line))?;
                    }
                    AstNode::NodeIndexExpression(index_node) => {
                        let (index_arg, _ty) = visit_const_expr(&index_node.index, scopes)?;
                        match (&index_node.name.node, index_arg) {
                            (AstNode::NodeIdentifier(ty), OpcodeArgument::INT(count)) => {
                                ensure!(
                                    count > 0,
                                    "Wrong array count {count} at line {}",
                                    this_node.line
                                );

                                scopes
                                    .register_array(
                                        arg_name.clone(),
                                        ty.into(),
                                        count as usize,
                                        VarType::Local,
                                    )
                                    .map_err(|e| anyhow!("{e} at line {}", this_node.line))?;
                            }
                            (AstNode::NodeIdentifier(_), other_index_node) => {
                                bail!(
                                    "Expected integer number, got {other_index_node} at line {}",
                                    this_node.line
                                )
                            }
                            _ => {
                                bail!("Unsupported node type {index_node} in function argument at line {}", this_node.line);
                            }
                        }
                    }
                    _ => bail!(
                        "Expected identifier, found {arg_name} at line {}",
                        this_node.line
                    ),
                }
            }

            for stmt in scope {
                visit_stmt(&stmt, emit, scopes)?;
            }

            scopes.exit();
        }
        AstNode::NodeConstDeclStml(inner_node) => {
            let values = visit_const_exprs(&inner_node.expr, scopes)?;
            let scope = scopes.get_current_scope();
            for (name, (_, arg, _)) in inner_node.names.iter().zip(values.iter()) {
                let line = name.line;
                match &name.node {
                    AstNode::NodeIdentifier(name) => {
                        scope
                            .register_const(name, arg.clone(), line)
                            .map_err(|e| anyhow!("{e} at line {}", this_node.line))?;
                    }
                    x => {
                        bail!("Expected identifier, found {x} at line {}", this_node.line)
                    }
                }
            }
        }
        AstNode::NodeVarDeclStml(inner_node) => {
            let mut names = vec![];

            for node in &inner_node.names {
                match &node.node {
                    AstNode::NodeIdentifier(name) => {
                        scopes
                            .register_var(name.to_string(), inner_node.ty, VarType::HeapAllocated)
                            .map_err(|e| anyhow!("{e} at line {}", this_node.line))?;
                        let inst = Instruction::VarDecl(VarDecl {
                            name: name.clone(),
                            ty: inner_node.ty,
                        });
                        emit(inst);

                        // single variables
                        names.push(node.clone());
                    }
                    AstNode::NodeIndexExpression(index_node) => {
                        let (index_arg, _ty) = visit_const_expr(&index_node.index, scopes)?;
                        match (&index_node.name.node, index_arg) {
                            (AstNode::NodeIdentifier(arr_name), OpcodeArgument::INT(count)) => {
                                ensure!(
                                    count > 0,
                                    "Wrong array count {count} at line {}",
                                    this_node.line
                                );

                                scopes
                                    .register_array(
                                        arr_name.to_string(),
                                        inner_node.ty,
                                        count as usize,
                                        VarType::HeapAllocated,
                                    )
                                    .map_err(|e| anyhow!("{e} at line {}", this_node.line))?;
                                let inst = Instruction::ArrayDecl(ArrayDecl {
                                    name: arr_name.clone(),
                                    ty: inner_node.ty,
                                    count,
                                });
                                emit(inst);

                                // array elements initialization
                                for i in 0..count {
                                    let node = AstNode::NodeIndexExpression(Box::new(
                                        NodeIndexExpression {
                                            name: index_node.name.clone(),
                                            index: node_span_with(
                                                AstNode::NodeIntLiteral(i as i32),
                                                index_node.index.line,
                                                index_node.index.start,
                                            ),
                                        },
                                    ));
                                    names.push(node_span_with(
                                        node,
                                        index_node.name.line,
                                        index_node.name.start,
                                    ));
                                }
                            }
                            (AstNode::NodeIdentifier(_), other_index_node) => {
                                bail!(
                                    "Expected integer number, got {other_index_node} at line {}",
                                    this_node.line
                                )
                            }
                            _ => {
                                bail!("Unsupported node type {index_node} in variable declaration at line {}", this_node.line);
                            }
                        }
                    }
                    other_node => bail!(
                        "Unsupported node type {other_node} in variable declaration at line {}",
                        this_node.line
                    ),
                }
            }

            if let Some(expr) = &inner_node.expr {
                let stmt = node_span_with(
                    AstNode::NodeAssignmentStmt(Box::new(crate::parser::NodeAssignmentStmt {
                        lhs: node_span_with(
                            AstNode::NodeList(names),
                            this_node.line,
                            this_node.start,
                        ),
                        rhs: expr.clone(),
                    })),
                    this_node.line,
                    this_node.start,
                );

                visit_stmt(&stmt, emit, scopes)?;
            }
        }
        AstNode::NodeDefaultReturnStmt => {
            if scopes.get_current_scope().is_root_scope() {
                emit(Instruction::OpcodeInst(OpcodeInst {
                    id: OP_TERMINATE_SCRIPT,
                    args: vec![],
                    is_variadic: false,
                }));
            } else {
                emit(Instruction::OpcodeInst(OpcodeInst {
                    id: OP_CLEO_RETURN_FAIL,
                    args: vec![],
                    is_variadic: true,
                }));
            }
        }
        AstNode::NodeReturnStmt(inner_node) => {
            ensure!(
                scopes.get_current_scope().is_function_scope(),
                "Return statement outside of function at line {}",
                this_node.line
            );

            let mut args = vec![];

            let name = match scopes.get_current_scope().get_type() {
                ScopeType::InternalFunction(name) | ScopeType::ExportedFunction(name) => name,
                _ => {
                    bail!("Unexpected scope at line {}", this_node.line);
                }
            };

            let name = name.clone();
            let Some(sig) = scopes.get_function_signature(&name) else {
                bail!("Unknown function '{}' at line {}", name, this_node.line);
            };

            if sig.is_logical {
                let Some(ref expr) = inner_node.expr else {
                    bail!("Expected logical expression at line {}", this_node.line);
                };
                visit_logical_expr(expr, emit, scopes, false)?;
                let inst = Instruction::OpcodeInst(OpcodeInst {
                    id: OP_CLEO_RETURN,
                    args: vec![OpcodeArgument::INT(0)],
                    is_variadic: true,
                });
                emit(inst);
            } else {
                let args_with_types = match &inner_node.expr {
                    Some(ref expr) => visit_exprs(expr, emit, scopes)?,
                    None => vec![],
                };

                ensure!(
                    (sig.is_optional && args_with_types.len() == 0)
                        || (sig.output_count == args_with_types.len()),
                    "Function '{}' returns {} values, but {} found at line {}",
                    name,
                    sig.output_count,
                    args_with_types.len(),
                    this_node.line
                );

                if args_with_types.is_empty() {
                    // returning any values sets the first argument of 2002 to 1 (true), or 0 (false) otherwise
                    args.push(OpcodeArgument::INT(0));
                } else {
                    args.push(OpcodeArgument::INT(1));
                }

                // validate returned types match function signature
                for (i, (expr, arg, ty)) in args_with_types.iter().enumerate() {
                    ensure!(
                        match_types(sig.output[i], *ty),
                        "Incompatible types: {}({}) and {} at line {}",
                        *ty,
                        expr,
                        sig.output[i],
                        this_node.line
                    );

                    deallocate_temp_var(arg, scopes)?;
                    args.push(arg.clone());
                }

                let inst = Instruction::OpcodeInst(OpcodeInst {
                    id: OP_CLEO_RETURN_WITH,
                    args,
                    is_variadic: true,
                });
                emit(inst);
            }
        }
        AstNode::NodeAssignmentStmt(node) => {
            let mut offload_assignments = vec![];
            let mut value_count = 0;
            visit_assignment_exprs(
                &node.rhs,
                emit,
                scopes,
                &mut |value_node, value_ty, emit, scopes| {
                    let Some(var_name) = (match &node.lhs.node {
                        AstNode::NodeList(ref names) => names.get(value_count),
                        _ if value_count == 0 => Some(&node.lhs),
                        _ => None,
                    }) else {
                        bail!(
                            "Not enough variables to store the result of {} at line {}",
                            value_node,
                            value_node.line
                        )
                    };
                    value_count += 1;
                    let var_type = get_node_type(&var_name, scopes)?;
                    ensure!(match_types(var_type, value_ty), "Incompatible types: {var_type}({var_name}) and {value_ty}({value_node}) at line {}", this_node.line);

                    let mut offload = |expr: OpcodeArgument,
                                       emit: &mut Emitter,
                                       scopes: &mut Scopes|
                     -> Result<OpcodeArgument> {
                        // check if any variables to the right are using current value (variable) (a,b=b,a)
                        let is_mutual_assignment = match &node.lhs.node {
                            AstNode::NodeList(ref names) => {
                                names.iter().skip(value_count).any(|name| {
                                    match (&name.node, &value_node.node) {
                                        (
                                            AstNode::NodeIndexExpression(var_index),
                                            AstNode::NodeIndexExpression(value_index),
                                        ) => var_index.name.node == value_index.name.node,
                                        (
                                            AstNode::NodeIdentifier(var),
                                            AstNode::NodeIdentifier(value),
                                        ) => var == value,
                                        _ => false,
                                    }
                                })
                            }
                            _ => false,
                        };
                        if is_mutual_assignment {
                            let target = default_allocator(this_node, var_type, emit, scopes)?;

                            let inst = Instruction::OpcodeInst(OpcodeInst {
                                id: get_assignment_opcode(&var_type, value_node)?,
                                args: vec![expr, target.clone()],
                                is_variadic: false,
                            });
                            offload_assignments.push(inst);

                            Ok(target)
                        } else {
                            Ok(expr)
                        }
                    };

                    let target = match &var_name.node {
                        AstNode::NodeIdentifier(_) => {
                            let target = node_to_argument(var_name, scopes)?;
                            offload(target, emit, scopes)?
                        }
                        AstNode::NodeIndexExpression(inner_node) => {
                            // *ptr = val
                            if matches!(var_type, ArgType::PInt32) {
                                match &value_node.node {
                                    AstNode::NodeIntLiteral(_)
                                    | AstNode::NodeFloatLiteral(_)
                                    | AstNode::NodeLabelLiteral(_)
                                    | AstNode::NodeIdentifier(_) => {
                                        let mut args =
                                            visit_pointer_expr(inner_node, emit, scopes)?;
                                        args.push(node_to_argument(&value_node, scopes)?);
                                        deallocate_temp_var(&args[1], scopes)?;
                                        let inst = Instruction::OpcodeInst(OpcodeInst {
                                            id: OP_WRITE_MEMORY_WITH_OFFSET,
                                            args,
                                            is_variadic: false,
                                        });
                                        emit(inst);

                                        bail!("skip");
                                    }
                                    _ => {
                                        let target = default_allocator(
                                            this_node,
                                            ArgType::Int,
                                            emit,
                                            scopes,
                                        )?;
                                        let mut args =
                                            visit_pointer_expr(inner_node, emit, scopes)?;
                                        args.push(target.clone());
                                        let inst = Instruction::OpcodeInst(OpcodeInst {
                                            id: OP_WRITE_MEMORY_WITH_OFFSET,
                                            args,
                                            is_variadic: false,
                                        });
                                        offload_assignments.push(inst);
                                        target
                                    }
                                }
                            } else {
                                let target = visit_index_expr(inner_node, emit, scopes)?.0;
                                offload(target, emit, scopes)?
                            }
                        }
                        _ => {
                            bail!("Invalid assignment target at line {}", this_node.line);
                        }
                    };
                    Ok(target)
                },
            )?;

            let var_count = match &node.lhs.node {
                AstNode::NodeList(ref names) => names.len(),
                _ => 1,
            };
            ensure!(
                var_count == value_count,
                "Expected {} values, got {} at line {}",
                var_count,
                value_count,
                this_node.line
            );

            for stmt in offload_assignments {
                if let Instruction::OpcodeInst(ref inst) = stmt {
                    for arg in &inst.args {
                        deallocate_temp_var(&arg, scopes)?;
                    }
                    emit(stmt);
                }
            }
        }
        AstNode::NodeIfStatement(inner_node) => {
            let else_label = scopes.unique_label();
            let exit_label = scopes.unique_label();

            visit_logical_expr(&inner_node.cond, emit, scopes, false)?;
            emit(op!(OP_JF => OpcodeArgument::LABEL(else_label.clone())));

            // then block
            for stmt in &inner_node.then_block {
                visit_stmt(&stmt, emit, scopes)?;
            }
            if inner_node.else_block.is_some() {
                emit(op!(OP_JUMP => OpcodeArgument::LABEL(exit_label.clone())));
            }

            // optional else block
            emit(label!(else_label));

            if let Some(else_block) = &inner_node.else_block {
                for stmt in else_block {
                    visit_stmt(&stmt, emit, scopes)?;
                }
            }

            // end of if statement
            emit(label!(exit_label));
        }
        AstNode::NodeCallExpression(inner_node) => {
            let (results, _) = visit_call_expr(
                inner_node,
                emit,
                scopes,
                &mut default_allocator,
                this_node,
                false,
            )?;

            for (target, _ty) in results {
                deallocate_temp_var(&target, scopes)?;
            }
        }
        AstNode::NodeWhileStatement(inner_node) => {
            let start_label = scopes.unique_label();
            let exit_label = scopes.unique_label();

            emit(label!(start_label));
            visit_logical_expr(&inner_node.cond, emit, scopes, false)?;
            emit(op!(OP_JF => OpcodeArgument::LABEL(exit_label.clone())));

            scopes
                .get_current_scope()
                .push_loop_labels(start_label.clone(), exit_label.clone());

            for stmt in &inner_node.body {
                visit_stmt(&stmt, emit, scopes)?;
            }

            emit(op!(OP_JUMP => OpcodeArgument::LABEL(start_label.clone())));
            emit(label!(exit_label));
            scopes.get_current_scope().pop_loop_labels();
        }
        AstNode::NodeBreakStatement => {
            let scope = scopes.get_current_scope();

            let Some((_, exit_label)) = scope.get_loop_labels() else {
                bail!("Break statement outside of loop at line {}", this_node.line);
            };
            emit(op!(OP_JUMP => OpcodeArgument::LABEL(exit_label.clone())));
        }
        AstNode::NodeContinueStatement => {
            let scope = scopes.get_current_scope();

            let Some((start_label, _)) = scope.get_loop_labels() else {
                bail!(
                    "Continue statement outside of loop at line {}",
                    this_node.line
                );
            };

            emit(op!(OP_JUMP => OpcodeArgument::LABEL(start_label.clone())));
        }
        other_node => bail!(
            "Expected declaration or statement, found {other_node} at line {}",
            this_node.line
        ),
    }

    Ok(())
}

fn visit_call_expr<'a, Emitter, Allocator>(
    inner_node: &'a NodeCallExpression,
    emit: &mut Emitter,
    scopes: &mut Scopes,
    allocate: &mut Allocator,
    this_node: &'a AstNodeSpan,
    negate_op: bool,
) -> Result<(Vec<(OpcodeArgument, ArgType)>, Function)>
where
    Emitter: FnMut(Instruction) -> (),
    Allocator: FnMut(&AstNodeSpan, ArgType, &mut Emitter, &mut Scopes) -> Result<OpcodeArgument>,
{
    let mut exprs = vec![];
    let Some(mut sig) = scopes.get_function_signature(&inner_node.name.to_string()) else {
        bail!(
            "Unknown function '{}' at line {}",
            inner_node.name,
            inner_node.name.line
        );
    };

    let mut args = vec![];
    let args_with_types = match &inner_node.args {
        Some(ref expr) => visit_exprs(expr, emit, scopes)?,
        None => vec![],
    };

    if sig.is_variadic {
        let min = sig.input_count - 1;
        let max = crate::scope::MAX_LVARS;
        ensure!(
            args_with_types.len() >= min,
            "Variadic function '{}' expected at least {} arguments, got {} at line {}",
            inner_node.name,
            min,
            args_with_types.len(),
            inner_node.name.line
        );
        let extra_args_count = args_with_types.len() - min;
        ensure!(
            extra_args_count <= max,
            "Too many arguments for variadic function '{}', expected up to {} arguments, got {} at line {}",
            inner_node.name,
            max,
            extra_args_count,
            inner_node.name.line
        );
    } else {
        ensure!(
            args_with_types.len() == sig.input_count,
            "Function '{}' expected {} arguments, got {} at line {}",
            inner_node.name,
            sig.input_count,
            args_with_types.len(),
            inner_node.name.line
        );
    }

    for (i, (expr, arg, ty)) in args_with_types.iter().enumerate() {
        if !sig.is_variadic || i < sig.input_count - 1 {
            ensure!(
                match_types(sig.input[i], *ty),
                "Incompatible types: {}({}) and {} at line {}",
                *ty,
                expr,
                sig.input[i],
                expr.line
            );
        };

        deallocate_temp_var(arg, scopes)?;
        args.push(arg.clone());
    }

    for i in 0..sig.output_count {
        let target = allocate(this_node, sig.output[i], emit, scopes)?;
        exprs.push((target.clone(), sig.output[i]));

        if sig.output_first {
            args.insert(0, target);
        } else {
            args.push(target);
        }
    }

    if sig.op == OP_CLEO_CALL {
        // function name is the label
        let offset = OpcodeArgument::LABEL(inner_node.name.to_string());

        //pass 0@ from parent scope which contains a pointer to allocated heap
        args.insert(0, OpcodeArgument::LVAR(0, ArgType::Int));
        args.insert(0, (sig.input_count + 1).into());

        args.insert(0, offset);
        sig.is_variadic = true;
    }

    emit(Instruction::OpcodeInst(OpcodeInst {
        args,
        id: if negate_op { NOT!(sig.op) } else { sig.op },
        is_variadic: sig.is_variadic,
    }));

    Ok((exprs, sig))
}

fn visit_pointer_expr<Emitter>(
    inner_node: &NodeIndexExpression,
    emit: &mut Emitter,
    scopes: &mut Scopes,
) -> Result<Vec<OpcodeArgument>>
where
    Emitter: FnMut(Instruction) -> (),
{
    let (index_expr, index_type) =
        visit_expr(&inner_node.index, emit, scopes, &mut default_allocator)?;
    ensure!(
        matches!(index_type, ArgType::Int),
        "Unsupported index {} with type {} at line {}",
        inner_node.index,
        index_type,
        inner_node.index.line
    );
    let ty = get_node_type(&inner_node.name, scopes)?;

    // check pointers before array validation because they are not registered as arrays
    match ty {
        ArgType::PInt32 => {
            // todo: create ArgType::Pointer(Argtype)
            let unit_size = 4; // sizeof(int)
            let offset = match index_expr {
                OpcodeArgument::INT(v) => OpcodeArgument::INT(v * unit_size),
                _ => {
                    let offset_var =
                        default_allocator(&inner_node.index, ArgType::Int, emit, scopes)?;
                    emit(Instruction::OpcodeInst(OpcodeInst {
                        id: OP_INT_MUL,
                        args: vec![
                            index_expr,
                            OpcodeArgument::INT(unit_size),
                            offset_var.clone(),
                        ],
                        is_variadic: false,
                    }));

                    offset_var
                }
            };
            return Ok(vec![
                node_to_argument(&inner_node.name, scopes)?,
                offset,
                OpcodeArgument::INT(unit_size),
            ]);
        }
        _ => {
            unreachable!()
        }
    }
}

fn visit_index_expr<Emitter>(
    inner_node: &NodeIndexExpression,
    emit: &mut Emitter,
    scopes: &mut Scopes,
) -> Result<(OpcodeArgument, ArgType)>
where
    Emitter: FnMut(Instruction) -> (),
{
    let AstNode::NodeIdentifier(array_name) = &inner_node.name.node else {
        bail!(
            "{} is not an array at line {}",
            inner_node.name,
            inner_node.name.line
        );
    };

    let Some(array) = scopes.get_current_scope().find_var(&array_name).cloned() else {
        bail!(
            "{array_name} is not an array at line {}",
            inner_node.name.line
        )
    };
    let (index_arg, index_type) =
        visit_expr(&inner_node.index, emit, scopes, &mut default_allocator)?;

    ensure!(
        matches!(index_type, ArgType::Int),
        "Unsupported index {} with type {} at line {}",
        inner_node.index,
        index_type,
        inner_node.index.line
    );

    // check pointers before array validation because they are not registered as arrays
    match array.ty {
        ArgType::PInt32 => {
            // todo: create ArgType::Pointer(Argtype)
            let target = default_allocator(&inner_node.index, ArgType::Int, emit, scopes)?;
            let mut args = visit_pointer_expr(inner_node, emit, scopes)?;
            args.push(target.clone());
            deallocate_temp_var(&args[1], scopes)?;
            emit(Instruction::OpcodeInst(OpcodeInst {
                id: OP_READ_MEMORY_WITH_OFFSET,
                args,
                is_variadic: false,
            }));
            return Ok((target, ArgType::Int));
        }
        _ => {}
    }

    ensure!(
        array.count > 1,
        "{array_name} is not an array at line {}",
        inner_node.name.line
    );

    let arg = match index_arg {
        OpcodeArgument::INT(i) => {
            ensure!(
                i >= 0,
                "Negative index {i} at line {}",
                inner_node.index.line
            );

            ensure!(
                i < array.count as i32,
                "Out of bound index {i} in array of size {} at line {}",
                array.count,
                inner_node.index.line
            );

            let unit_size = crate::scope::get_type_size(&array.ty);

            match array.var_type {
                VarType::HeapAllocated => OpcodeArgument::ARRAY(Box::new((
                    OpcodeArgument::GVAR(array.index + i as usize * unit_size, array.ty),
                    OpcodeArgument::LVAR(0, ArgType::Int),
                    1,
                ))),
                VarType::Local => {
                    OpcodeArgument::LVAR(array.index + i as usize * unit_size, array.ty)
                }
            }
        }

        _ => {
            match array.var_type {
                VarType::HeapAllocated => {
                    let ty = ArgType::Int;
                    let temp_index = scopes.allocate_local_var(ty, 1)?;
                    let temp_index_var = OpcodeArgument::LVAR(temp_index, ty);

                    // temp_index_var = heap (0@) + index
                    emit(Instruction::OpcodeInst(OpcodeInst {
                        id: get_assignment_opcode_binary(&ty, &Token::Add)
                            .map_err(|e| anyhow!("{e} at line {}", inner_node.name.line))?,
                        args: vec![
                            OpcodeArgument::LVAR(0, ty), // heap pointer
                            index_arg.clone(),
                            temp_index_var.clone(),
                        ],
                        is_variadic: false,
                    }));

                    OpcodeArgument::ARRAY(Box::new((
                        OpcodeArgument::GVAR(array.index, array.ty),
                        temp_index_var,
                        1, // arr_size,
                    )))
                }
                VarType::Local if matches!(index_arg, OpcodeArgument::LVAR(..)) => {
                    OpcodeArgument::ARRAY(Box::new((
                        OpcodeArgument::LVAR(array.index, array.ty),
                        index_arg.clone(),
                        1, // arr_size,
                    )))
                }
                VarType::Local => {
                    let ty = ArgType::Int;
                    let temp_index = scopes.allocate_local_var(ty, 1)?;
                    let temp_index_var = OpcodeArgument::LVAR(temp_index, ty);

                    // temp_index_var = index
                    emit(Instruction::OpcodeInst(OpcodeInst {
                        id: get_assignment_opcode(&ty, &inner_node.index)
                            .map_err(|e| anyhow!("{e} at line {}", inner_node.name.line))?,
                        args: vec![temp_index_var.clone(), index_arg.clone()],
                        is_variadic: false,
                    }));

                    OpcodeArgument::ARRAY(Box::new((
                        OpcodeArgument::LVAR(array.index, array.ty),
                        temp_index_var,
                        1, // arr_size,
                    )))
                }
            }
        }
    };
    Ok((arg, array.ty))
}

fn get_assignment_opcode(lhs_ty: &ArgType, rhs_node: &AstNodeSpan) -> Result<u16> {
    match lhs_ty {
        ArgType::Int | ArgType::PInt32 => match &rhs_node.node {
            AstNode::NodeIntLiteral(_) | AstNode::NodeLabelLiteral(_) => Ok(OP_SET_LVAR_INT),
            _ => Ok(OP_SET_LVAR_INT),
        },
        ArgType::Float => match &rhs_node.node {
            AstNode::NodeFloatLiteral(_) => Ok(OP_SET_LVAR_FLOAT),
            _ => Ok(OP_SET_LVAR_FLOAT),
        },
        ArgType::Void => bail!(
            "Void type at line {}. Did you mean {rhs_node}()?",
            rhs_node.line,
        ),
        ArgType::String => match &rhs_node.node {
            _ => Ok(OP_SET_LVAR_TEXT_LABEL16),
        },
    }
}

fn get_assignment_opcode_binary(ty: &ArgType, op: &Token) -> Result<u16> {
    match ty {
        ArgType::Int | ArgType::PInt32 => match op {
            Token::Add => Ok(OP_INT_ADD),
            Token::Sub => Ok(OP_INT_SUB),
            Token::Mul => Ok(OP_INT_MUL),
            Token::Div => Ok(OP_INT_DIV),
            Token::BitAnd => Ok(0x0B10),
            Token::BitOr => Ok(0x0B11),
            Token::BitXor => Ok(0x0B12),
            _ => bail!("Invalid operator {op}"),
        },
        ArgType::Float => match op {
            // Token::Add => Ok(0x2705),
            // Token::Sub => Ok(0x2706),
            // Token::Mul => Ok(0x2707),
            // Token::Div => Ok(0x2708),
            _ => bail!("Invalid operator {op}"),
        },
        ArgType::Void => bail!("Void type not allowed in binary operation"),
        ArgType::String => {
            bail!("String type not allowed in binary operation")
        }
    }
}

fn get_logical_opcode(ty: &ArgType, op: &Token) -> Result<u16> {
    match ty {
        ArgType::Int | ArgType::PInt32 => match op {
            Token::Greater => Ok(0x0019),
            Token::GreaterEqual => Ok(0x0029),
            Token::EqualEqual => Ok(0x0039),
            Token::LessEqual => Ok(NOT!(0x0019)),
            Token::Less => Ok(NOT!(0x0029)),
            Token::NotEqual => Ok(NOT!(0x0039)),

            _ => bail!("Invalid operator {op}"),
        },
        ArgType::Float => match op {
            Token::Greater => Ok(0x0021),
            Token::GreaterEqual => Ok(0x0031),
            Token::EqualEqual => Ok(0x0043),
            Token::LessEqual => Ok(NOT!(0x0021)),
            Token::Less => Ok(NOT!(0x0031)),
            Token::NotEqual => Ok(NOT!(0x0043)),

            _ => bail!("Invalid operator {op}"),
        },
        ArgType::Void => bail!("Void type not allowed in logical operation"),
        ArgType::String => bail!("String type not allowed in logical operation"),
    }
}

fn get_node_type<'a>(node: &AstNodeSpan, scopes: &mut Scopes) -> Result<ArgType> {
    fn get_node_type_internal<'a>(node: &AstNode, scopes: &mut Scopes) -> Result<ArgType> {
        match node {
            AstNode::NodeIntLiteral(_) | AstNode::NodeLabelLiteral(_) => Ok(ArgType::Int),
            AstNode::NodeFloatLiteral(_) => Ok(ArgType::Float),
            AstNode::NodeIdentifier(name) => {
                if let Some(v) = scopes.get_current_scope().find_constant(&name) {
                    let ty = match v {
                        OpcodeArgument::INT(_) | OpcodeArgument::LABEL(_) => ArgType::Int,
                        OpcodeArgument::FLOAT(_) => ArgType::Float,
                        OpcodeArgument::STR(_) => ArgType::String,
                        _ => {
                            bail!("Unexpected const type {}", v)
                        }
                    };
                    return Ok(ty);
                }

                match scopes.get_current_scope().find_var(name) {
                    Some(v) => Ok(v.ty),
                    None => Ok(ArgType::Void),
                }
            }

            AstNode::NodeIndexExpression(inner_node) => {
                get_node_type_internal(&inner_node.name.node, scopes)
            }
            AstNode::Root(_)
            | AstNode::NodeList(_)
            | AstNode::NodeBinary(_)
            | AstNode::NodeReturnStmt(_)
            | AstNode::NodeVarDeclStml(_)
            | AstNode::NodeFuncDeclStml(_)
            | AstNode::NodeConstDeclStml(_)
            | AstNode::NodeDefaultReturnStmt
            | AstNode::NodeIfStatement(_)
            | AstNode::NodeWhileStatement(_)
            | AstNode::NodeBreakStatement
            | AstNode::NodeContinueStatement
            | AstNode::NodeAssignmentStmt(_) => Ok(ArgType::Void),
            AstNode::NodeCallExpression(inner_node) => {
                match scopes
                    .get_current_scope()
                    .find_function(&inner_node.name.to_string())
                {
                    Some(f) => {
                        ensure!(
                            f.output_count == 1, //|| f.is_logical,
                            "A function should have 1 output param to be used in an expression. '{}' has {}",
                            inner_node.name,
                            f.output_count
                        );
                        Ok(f.output[0])
                    }
                    None => Ok(ArgType::Void),
                }
            }
            AstNode::NodeStringLiteral(_) => Ok(ArgType::String),
            AstNode::NodeUnary(v) if v.op == Token::BitNot => Ok(ArgType::Int),
            AstNode::NodeUnary(_) => Ok(ArgType::Void),
        }
    }

    get_node_type_internal(&node.node, scopes).map_err(|e| anyhow!("{e} at line {}", node.line))
}

/// finds all functions defined in current scope and moves them at the bottom to avoid control flow hitting them
/// todo: consider adding a breakpoint before function body with a warning
fn create_scope<'a>(
    stmts: &'a Vec<AstNodeSpan>,
    scopes: &mut Scopes,
) -> Result<Vec<&'a AstNodeSpan>> {
    let mut scope = vec![];
    let mut functions = vec![];

    for stmt in stmts {
        if let AstNode::NodeFuncDeclStml(v) = &stmt.node {
            let mut input = [ArgType::Void; crate::scope::MAX_LVARS];
            let mut input_count = 0;
            loop {
                if input_count >= v.args.len() {
                    break;
                }
                match &v.args[input_count].1.node {
                    AstNode::NodeIdentifier(ty) => {
                        input[input_count] = ty.into();
                        input_count += 1;
                    }
                    AstNode::NodeIndexExpression(index_node) => {
                        let (index_arg, _ty) = visit_const_expr(&index_node.index, scopes)?;
                        match (&index_node.name.node, index_arg) {
                            (AstNode::NodeIdentifier(ty), OpcodeArgument::INT(count)) => {
                                for _ in 0..count {
                                    input[input_count] = ty.into();
                                    input_count += 1;
                                }
                            }
                            _ => {
                                bail!("Unsupported node type {index_node} in function argument at line {}", stmt.line);
                            }
                        }
                    }
                    x => bail!("Expected identifier, found {x} at line {}", stmt.line),
                }
            }

            let f = Function {
                input_count,
                input,
                output_count: v.return_types.len(),
                output: {
                    let mut array = [ArgType::Void; 32];
                    for (i, ty) in v.return_types.iter().enumerate() {
                        array[i] = ty.into();
                    }
                    array
                },
                is_optional: v.is_optional,
                is_logical: v.is_logical,
                op: OP_CLEO_CALL,

                is_variadic: false,
                output_first: false,
            };

            scopes
                .get_current_scope()
                .register_fn(&v.name, f, stmt.line)
                .map_err(|e| anyhow!("{e} at line {}", stmt.line))?;
            functions.push(stmt);

            if v.is_exported {
                scopes.export_function(v.name.clone(), f);
            }
        } else {
            scope.push(stmt);
        }
    }
    scope.append(&mut functions);
    Ok(scope)
}

fn node_to_argument(node: &AstNodeSpan, scopes: &mut Scopes) -> Result<OpcodeArgument> {
    let argument = match &node.node {
        AstNode::NodeIntLiteral(i) => OpcodeArgument::INT(*i),
        AstNode::NodeLabelLiteral(label) => OpcodeArgument::LABEL(label.clone()),
        AstNode::NodeFloatLiteral(f) => OpcodeArgument::FLOAT(*f),
        AstNode::NodeStringLiteral(s) => OpcodeArgument::STR(s.clone()),
        AstNode::NodeIdentifier(name) => {
            if let Some(v) = scopes.get_current_scope().find_constant(name) {
                return Ok(v.clone());
            }
            if let Some(v) = scopes.get_current_scope().find_var(name) {
                match v.var_type {
                    VarType::HeapAllocated => OpcodeArgument::ARRAY(Box::new((
                        OpcodeArgument::GVAR(v.index, v.ty),
                        OpcodeArgument::LVAR(0, ArgType::Int),
                        1,
                    ))),
                    VarType::Local => OpcodeArgument::LVAR(v.index, v.ty),
                }
            } else {
                bail!(
                    "Unknown name {node} at line {}. Did you mean {node}()?",
                    node.line
                )
            }
        }
        _ => bail!(
            "Can't convert {node} to an argument. Node is not a literal at line {}",
            node.line
        ),
    };
    Ok(argument)
}

fn default_allocator<Emitter>(
    _node: &AstNodeSpan,
    ty: ArgType,
    _emit: &mut Emitter,
    scopes: &mut Scopes,
) -> Result<OpcodeArgument> {
    let index = scopes.allocate_local_var(ty, 1)?;
    Ok(OpcodeArgument::LVAR(index, ty))
}

fn deallocate_temp_var(arg: &OpcodeArgument, scopes: &mut Scopes) -> Result<()> {
    match arg {
        OpcodeArgument::ARRAY(v) => match (&v.0, &v.1) {
            (OpcodeArgument::GVAR(_gindex, _), OpcodeArgument::LVAR(lindex, _)) if *lindex > 0 => {
                scopes.deallocate_local_var(*lindex)?;
            }
            _ => {}
        },
        OpcodeArgument::LVAR(lindex, _) => {
            scopes.deallocate_local_var(*lindex)?;
        }
        _ => {}
    }

    Ok(())
}

fn match_types(ty1: ArgType, ty2: ArgType) -> bool {
    match (ty1, ty2) {
        (ty1, ty2) if ty1 == ty2 => true,
        (ArgType::Int, ArgType::PInt32) => true,
        (ArgType::PInt32, ArgType::Int) => true,
        _ => false,
    }
}
