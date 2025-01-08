use std::fmt::{Display, Formatter};

use crate::tokens::{Keyword, Token, TokenSpan};
use anyhow::{anyhow, bail, Result};

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    Root(Vec<AstNodeSpan>),

    NodeAssignmentStmt(Box<NodeAssignmentStmt>),
    NodeReturnStmt(Box<NodeReturnStmt>), // 2002
    NodeDefaultReturnStmt,               // 2003
    NodeVarDeclStml(Box<NodeVarDeclStml>),
    NodeFuncDeclStml(Box<NodeFuncDeclStml>),
    NodeConstDeclStml(Box<NodeConstDeclStml>),

    NodeIndexExpression(Box<NodeIndexExpression>),
    NodeCallExpression(Box<NodeCallExpression>),
    NodeUnary(Box<NodeUnary>),
    NodeBinary(Box<NodeBinary>),

    NodeList(Vec<AstNodeSpan>),

    NodeIntLiteral(i32),
    NodeFloatLiteral(f32),
    NodeLabelLiteral(String),
    NodeStringLiteral(Vec<u8>),
    // NodeString8Literal(Vec<u8>),
    // NodeString16Literal(Vec<u8>),
    NodeIdentifier(String),

    NodeIfStatement(Box<NodeIfStatement>),
    NodeWhileStatement(Box<NodeWhileStatement>),
    NodeBreakStatement,
    NodeContinueStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstNodeSpan {
    pub node: AstNode,
    pub line: usize,
    pub start: usize,
}

pub fn parse(tokens: Vec<TokenSpan>) -> Result<AstNodeSpan> {
    let mut index = 0;
    parse_block(&tokens, &mut index)
        .and_then(|statements| {
            expect(&tokens, &mut index, Token::Eof)
                .map(|_| node_span_with(AstNode::Root(statements), 1, 1))
        })
        .map_err(|e| {
            anyhow!(
                "{e} at line {} col {}",
                tokens[index].line,
                tokens[index].start
            )
        })
}

fn parse_block(tokens: &Vec<TokenSpan>, index: &mut usize) -> Result<Vec<AstNodeSpan>> {
    let mut statements = vec![];

    loop {
        match tokens.get(*index) {
            Some(TokenSpan {
                token: Token::Eof,
                line,
                start,
                end: _,
            }) => {
                statements.push(AstNodeSpan {
                    node: AstNode::NodeDefaultReturnStmt,
                    line: *line,
                    start: *start,
                });
                break;
            }
            Some(TokenSpan {
                token: Token::Kwd(Keyword::End) | Token::Kwd(Keyword::Else),
                line: _,
                start: _,
                end: _,
            }) => break,
            Some(TokenSpan {
                token: Token::NewLine,
                start: _,
                end: _,
                line: _,
            }) => {
                *index += 1;
                continue;
            }
            _ => {
                match parse_statements(&tokens, index) {
                    Ok(new_stmts) => {
                        statements.extend(new_stmts);
                    }
                    Err(e) => bail!(e),
                }

                match tokens.get(*index).map(|t| &t.token) {
                    Some(Token::Eof | Token::NewLine) => {
                        continue;
                    }
                    x => {
                        bail!(
                            "Expected a {}, got {}",
                            Token::NewLine,
                            x.unwrap_or(&Token::Eof)
                        );
                    }
                }
            }
        }
    }

    Ok(statements)
}

fn parse_function(
    tokens: &Vec<TokenSpan>,
    index: &mut usize,
    is_exported: bool,
) -> Result<AstNode> {
    let name = expect_ident(tokens, index)?.to_string();

    let mut args = vec![];

    if expect(tokens, index, Token::OpenParen).is_ok() {
        args = list(tokens, index, &|tokens,
                                     index|
         -> Result<(String, AstNodeSpan)> {
            match tokens.get(*index) {
                Some(TokenSpan {
                    token: Token::CloseParen,
                    line: _,
                    start: _,
                    end: _,
                }) => {
                    bail!("")
                }
                Some(TokenSpan {
                    token: Token::Ident(arg_name),
                    line,
                    start,
                    end: _,
                }) => {
                    *index += 1;
                    expect(tokens, index, Token::Colon)?;
                    let ty = expect_type(tokens, index)?;
                    // []
                    match tokens.get(*index) {
                        Some(TokenSpan {
                            token: Token::OpenBracket,
                            line: _,
                            start: _,
                            end: _,
                        }) => {
                            *index += 1; // [
                            let node =
                                AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                    name: node_span_with(
                                        AstNode::NodeIdentifier(ty),
                                        *line,
                                        *start,
                                    ),
                                    index: parse_expr(tokens, index, 0)?,
                                }));
                            expect(tokens, index, Token::CloseBracket)?; // ]
                            Ok((arg_name.clone(), node_span_with(node, *line, *start)))
                        }
                        _ => Ok((
                            arg_name.clone(),
                            node_span_with(AstNode::NodeIdentifier(ty.clone()), *line, *start),
                        )),
                    }
                }
                // todo: class names, enums
                other_type => {
                    bail!(
                        "Expected identifier, got {}",
                        other_type.map(|t| &t.token).unwrap_or(&Token::Eof)
                    )
                }
            }
        })?;

        expect(tokens, index, Token::CloseParen)?;
    }
    // result
    let mut is_optional = false;
    let mut is_logical = false;
    let mut return_types = vec![];
    if expect(tokens, index, Token::Colon).is_ok() {
        if let Ok(_) = expect_keyword(tokens, index, &|tok| tok == &Keyword::Logical) {
            is_logical = true;
        } else {
            if let Ok(_) = expect_keyword(tokens, index, &|tok| tok == &Keyword::Optional) {
                is_optional = true;
            };
            return_types = list(tokens, index, &|tokens, index| -> Result<String> {
                expect_type(tokens, index)
            })?;

            if return_types.is_empty() {
                bail!("Expected a return type")
            }
        }
    }
    expect(tokens, index, Token::NewLine)?;

    let mut body = parse_block(tokens, index)?;
    expect(&tokens, index, Token::Kwd(Keyword::End))?;

    // default return at `end` keyword
    match body.last() {
        Some(inst) if matches!(inst.node, AstNode::NodeReturnStmt(_)) => {}
        _ => {
            let node = node_span_with(
                AstNode::NodeDefaultReturnStmt,
                tokens[*index - 1].line,
                tokens[*index - 1].start,
            ); // 'end' token
            body.push(node);
        }
    }

    let node = AstNode::NodeFuncDeclStml(Box::new(NodeFuncDeclStml {
        name,
        body,
        args,
        is_optional,
        is_logical,
        is_exported,
        return_types,
    }));
    Ok(node)
}

fn parse_statements(tokens: &Vec<TokenSpan>, index: &mut usize) -> Result<Vec<AstNodeSpan>> {
    let Some(token) = tokens.get(*index) else {
        bail!("Expected a statement")
    };
    match &token.token {
        Token::Kwd(Keyword::Int) => {
            *index += 1;
            parse_var_decl(tokens, index, ArgType::Int)
                .map(|node| vec![node_span_with(node, token.line, token.start)])
        }
        Token::Kwd(Keyword::Float) => {
            *index += 1;
            parse_var_decl(tokens, index, ArgType::Float)
                .map(|node| vec![node_span_with(node, token.line, token.start)])
        }
        Token::Kwd(Keyword::String) => {
            *index += 1;
            parse_var_decl(tokens, index, ArgType::String)
                .map(|node| vec![node_span_with(node, token.line, token.start)])
        }
        Token::Kwd(Keyword::PInt32) => {
            *index += 1;
            parse_var_decl(tokens, index, ArgType::PInt32)
                .map(|node| vec![node_span_with(node, token.line, token.start)])
        }
        Token::Kwd(Keyword::Return) => {
            *index += 1;
            let expr = match tokens.get(*index).map(|t| &t.token) {
                Some(Token::Eof | Token::NewLine) => None,
                _ => Some(parse_expr(tokens, index, 0)?),
            };
            Ok(vec![node_span_with(
                AstNode::NodeReturnStmt(Box::new(NodeReturnStmt { expr })),
                token.line,
                token.start,
            )])
        }
        Token::Kwd(Keyword::Const) => {
            *index += 1;
            parse_const_decl(tokens, index)
                .map(|node| vec![node_span_with(node, token.line, token.start)])
        }
        Token::Kwd(Keyword::If) => {
            *index += 1;
            parse_if_statement(tokens, index)
                .map(|node| vec![node_span_with(node, token.line, token.start)])
        }
        Token::Kwd(Keyword::Function) => {
            *index += 1;
            parse_function(tokens, index, /* is_exported */ false)
                .map(|node| vec![node_span_with(node, token.line, token.start)])
        }
        Token::Kwd(Keyword::Export) => {
            *index += 1;
            match tokens.get(*index) {
                Some(TokenSpan {
                    token: Token::Kwd(Keyword::Function),
                    line: _,
                    start: _,
                    end: _,
                }) => {
                    *index += 1;
                    parse_function(tokens, index, /* is_exported */ true)
                        .map(|node| vec![node_span_with(node, token.line, token.start)])
                }
                _ => bail!("Expected a function declaration"),
            }
        }
        //while
        Token::Kwd(Keyword::While) => {
            *index += 1;
            let cond = parse_expr(tokens, index, 0)?;

            let body = parse_block(tokens, index)?;
            expect(tokens, index, Token::Kwd(Keyword::End))?;
            Ok(vec![node_span_with(
                AstNode::NodeWhileStatement(Box::new(NodeWhileStatement { cond, body })),
                token.line,
                token.start,
            )])
        }
        Token::Kwd(Keyword::Break) => {
            *index += 1;
            Ok(vec![node_span_with(
                AstNode::NodeBreakStatement,
                token.line,
                token.start,
            )])
        }
        Token::Kwd(Keyword::Continue) => {
            *index += 1;
            Ok(vec![node_span_with(
                AstNode::NodeContinueStatement,
                token.line,
                token.start,
            )])
        }
        _ => Ok(vec![parse_expr(tokens, index, 0)?]),
    }
}

fn parse_if_statement(tokens: &Vec<TokenSpan>, index: &mut usize) -> Result<AstNode> {
    let cond = parse_expr(tokens, index, 0)?;

    skip_new_lines(tokens, index);

    expect_keyword(tokens, index, &|kwd| matches!(kwd, Keyword::Then)).map_err(|other_type| {
        anyhow!(
            "Expected {} got {}",
            &Keyword::Then,
            other_type.unwrap_or(&Token::Eof)
        )
    })?;

    // mandatory `then` block
    let then_block = parse_block(tokens, index)?;

    // optional `else` block
    let else_block = match expect_keyword(tokens, index, &|kwd| matches!(kwd, Keyword::Else)) {
        Ok(_) => Some(parse_block(tokens, index)?),
        _ => None,
    };

    expect(&tokens, index, Token::Kwd(Keyword::End))?;

    Ok(AstNode::NodeIfStatement(Box::new(NodeIfStatement {
        cond,
        then_block,
        else_block,
    })))
}

/// Parse list of identifiers, then match `=` with list of expressions and exit
fn parse_var_decl(tokens: &Vec<TokenSpan>, index: &mut usize, ty: ArgType) -> Result<AstNode> {
    let names = list(tokens, index, &|tokens, index| -> Result<AstNodeSpan> {
        let name = expect_ident(tokens, index)?;
        let (line, start) = (tokens[*index - 1].line, tokens[*index - 1].start);

        // inline array declaration: int name[size]
        match tokens.get(*index) {
            Some(TokenSpan {
                token: Token::OpenBracket,
                line: _,
                start: _,
                end: _,
            }) => {
                *index += 1; // [
                let name_node = AstNode::NodeIdentifier(name.to_string());
                let node = AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                    name: node_span_with(name_node, line, start),
                    index: parse_expr(tokens, index, 0)?,
                }));

                expect(tokens, index, Token::CloseBracket)?; // ]

                Ok(node_span_with(node, line, start))
            }
            _ => {
                let node = AstNode::NodeIdentifier(name.to_string());
                Ok(node_span_with(node, line, start))
            }
        }
    })?;

    let node = AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
        ty,
        expr: {
            match tokens.get(*index) {
                Some(TokenSpan {
                    token: Token::Equal,
                    line: _,
                    start: _,
                    end: _,
                }) => {
                    *index += 1;
                    Some(parse_expr(tokens, index, 0)?)
                }
                _ => None,
            }
        },
        names,
    }));

    Ok(node)
}

fn parse_const_decl(tokens: &Vec<TokenSpan>, index: &mut usize) -> Result<AstNode> {
    let names = list(tokens, index, &|tokens, index| -> Result<AstNodeSpan> {
        let name = expect_ident(tokens, index)?;
        let (line, start) = (tokens[*index - 1].line, tokens[*index - 1].start);

        let node = AstNode::NodeIdentifier(name.to_string());
        Ok(node_span_with(node, line, start))
    })?;

    expect(tokens, index, Token::Equal)?;

    let expr = parse_expr(tokens, index, 0)?;
    let node = AstNode::NodeConstDeclStml(Box::new(NodeConstDeclStml { names, expr }));

    Ok(node)
}

fn parse_expr(
    tokens: &Vec<TokenSpan>,
    index: &mut usize,
    min_precedence: u8,
) -> Result<AstNodeSpan> {
    let mut lhs = parse_term(tokens, index, ArgType::Void)?;
    let line = lhs.line;
    let start = lhs.start;
    loop {
        let Some(op) = tokens.get(*index).map(|t| &t.token) else {
            break;
        };
        match get_op_prec(op) {
            Some((prec, _)) if prec < min_precedence => {
                break;
            }
            Some((prec, assoc)) => {
                *index += 1;

                lhs = match op {
                    &Token::OpenBracket => parse_index_expression(tokens, lhs, index)
                        .map(|node| node_span_with(node, line, start))?,
                    &Token::OpenParen => parse_call_expression(tokens, lhs, index)
                        .map(|node| node_span_with(node, line, start))?,
                    _ => {
                        let rhs = parse_expr(
                            tokens,
                            index,
                            prec + if assoc == Assoc::Left { 1 } else { 0 },
                        );

                        match rhs {
                            Err(_) => bail!("Expected expression"),
                            Ok(rhs) => match op {
                                Token::Equal => node_span_with(
                                    AstNode::NodeAssignmentStmt(Box::new(NodeAssignmentStmt {
                                        lhs,
                                        rhs,
                                    })),
                                    line,
                                    start,
                                ),
                                Token::AddEqual
                                | Token::SubEqual
                                | Token::MulEqual
                                | Token::DivEqual
                                | Token::BitAndEqual
                                | Token::BitOrEqual
                                | Token::BitXorEqual => node_span_with(
                                    AstNode::NodeAssignmentStmt(Box::new(NodeAssignmentStmt {
                                        lhs: lhs.clone(),
                                        rhs: node_span_with(
                                            AstNode::NodeBinary(Box::new(NodeBinary {
                                                lhs,
                                                rhs,
                                                op: match op {
                                                    Token::AddEqual => Token::Add,
                                                    Token::SubEqual => Token::Sub,
                                                    Token::MulEqual => Token::Mul,
                                                    Token::DivEqual => Token::Div,
                                                    Token::BitAndEqual => Token::BitAnd,
                                                    Token::BitOrEqual => Token::BitOr,
                                                    Token::BitXorEqual => Token::BitXor,
                                                    _ => unreachable!("no other compound operators"),
                                                },
                                            })),
                                            line,
                                            start,
                                        ),
                                    })),
                                    line,
                                    start,
                                ),
                                Token::Comma => node_span_with(
                                    match lhs.node {
                                        AstNode::NodeList(mut nodes) => {
                                            nodes.push(rhs);
                                            AstNode::NodeList(nodes)
                                        }
                                        _ => AstNode::NodeList(vec![lhs, rhs]),
                                    },
                                    line,
                                    start,
                                ),
                                other => node_span_with(
                                    AstNode::NodeBinary(Box::new(NodeBinary {
                                        lhs,
                                        rhs,
                                        op: other.clone(),
                                    })),
                                    line,
                                    start,
                                ),
                            },
                        }
                    }
                };
            }
            None => break,
        }
    }

    Ok(lhs)
}

fn parse_call_expression(
    tokens: &Vec<TokenSpan>,
    lhs: AstNodeSpan,
    index: &mut usize,
) -> Result<AstNode> {
    let args = match expect(tokens, index, Token::CloseParen) {
        Ok(_) => None,
        Err(_) => {
            let args = parse_expr(tokens, index, 0)?;
            expect(tokens, index, Token::CloseParen)?;
            Some(args)
        }
    };

    Ok(AstNode::NodeCallExpression(Box::new(NodeCallExpression {
        name: lhs,
        args,
    })))
}

fn parse_index_expression(
    tokens: &Vec<TokenSpan>,
    lhs: AstNodeSpan,
    index: &mut usize,
) -> Result<AstNode> {
    let Ok(index_expr) = parse_expr(tokens, index, 0) else {
        bail!("Expected expression")
    };

    expect(tokens, index, Token::CloseBracket)?;

    Ok(AstNode::NodeIndexExpression(Box::new(
        NodeIndexExpression {
            name: lhs,
            index: index_expr,
        },
    )))
}

fn parse_term(tokens: &Vec<TokenSpan>, index: &mut usize, _ty: ArgType) -> Result<AstNodeSpan> {
    skip_new_lines(tokens, index);
    match tokens.get(*index) {
        Some(TokenSpan {
            token: Token::Ident(s),
            line,
            start,
            end: _,
        }) => {
            *index += 1;
            let node = AstNode::NodeIdentifier(s.clone());
            Ok(node_span_with(node, *line, *start))
        }
        Some(TokenSpan {
            token: Token::Kwd(key),
            line,
            start,
            end: _,
        }) if *key == Keyword::Int || *key == Keyword::Float => {
            *index += 1;
            let node = AstNode::NodeIdentifier(key.to_string());
            Ok(node_span_with(node, *line, *start))
        }
        Some(TokenSpan {
            token: Token::Int(i),
            line,
            start,
            end: _,
        }) => {
            *index += 1;
            let node = AstNode::NodeIntLiteral(*i);
            Ok(node_span_with(node, *line, *start))
        }
        Some(TokenSpan {
            token: Token::Float(f),
            line,
            start,
            end: _,
        }) => {
            *index += 1;
            let node = AstNode::NodeFloatLiteral(*f);
            Ok(node_span_with(node, *line, *start))
        }
        Some(TokenSpan {
            token: Token::Label(s),
            line,
            start,
            end: _,
        }) => {
            *index += 1;
            let node = AstNode::NodeLabelLiteral(s.clone());
            Ok(node_span_with(node, *line, *start))
        }
        Some(TokenSpan {
            token: Token::String(s),
            line,
            start,
            end: _,
        }) => {
            *index += 1;
            let node = AstNode::NodeStringLiteral(s.clone());
            Ok(node_span_with(node, *line, *start))
        }
        Some(TokenSpan {
            token: Token::BitNot,
            line,
            start,
            end: _,
        }) => {
            // ~expr
            *index += 1;
            let expr = parse_expr(tokens, index, get_op_prec(&Token::BitNot).unwrap().0);
            match expr {
                Ok(e) if e.node.is_statement() => {
                    bail!("Expected expression")
                }
                Ok(inner_node) => {
                    let node = AstNode::NodeUnary(Box::new(NodeUnary {
                        node: inner_node,
                        op: Token::BitNot,
                    }));

                    Ok(node_span_with(node, *line, *start))
                }
                Err(e) => bail!(e),
            }
        }
        Some(TokenSpan {
            token: Token::Not,
            line,
            start,
            end: _,
        }) => {
            // not expr
            *index += 1;
            let expr = parse_expr(tokens, index, get_op_prec(&Token::Not).unwrap().0);
            match expr {
                Ok(e) if e.node.is_statement() => {
                    bail!("Expected expression")
                }
                Ok(inner_node) => {
                    let node = AstNode::NodeUnary(Box::new(NodeUnary {
                        node: inner_node,
                        op: Token::Not,
                    }));

                    Ok(node_span_with(node, *line, *start))
                }
                Err(e) => bail!(e),
            }
        }
        Some(TokenSpan {
            token: Token::Sub,
            line: _,
            start: _,
            end: _,
        }) => {
            //unary minus
            *index += 1;
            let expr = parse_expr(tokens, index, get_op_prec(&Token::BitNot).unwrap().0); // unary not and unary minus have same precendence
            match expr {
                Ok(AstNodeSpan {
                    node: AstNode::NodeIntLiteral(i),
                    line,
                    start,
                }) => Ok(node_span_with(AstNode::NodeIntLiteral(-i), line, start)),
                Ok(AstNodeSpan {
                    node: AstNode::NodeFloatLiteral(f),
                    line,
                    start,
                }) => Ok(node_span_with(AstNode::NodeFloatLiteral(-f), line, start)),
                Ok(e) if e.node.is_statement() => {
                    bail!("Expected expression")
                }
                Ok(AstNodeSpan { node, line, start }) => {
                    let node = AstNode::NodeBinary(Box::new(NodeBinary {
                        lhs: node_span_with(AstNode::NodeIntLiteral(-1), line, start),
                        rhs: node_span_with(node, line, start),
                        op: Token::Mul,
                    }));
                    Ok(node_span_with(node, line, start))
                }
                Err(e) => bail!(e),
            }
        }
        Some(TokenSpan {
            token: Token::OpenParen,
            line: _,
            start: _,
            end: _,
        }) => {
            *index += 1;
            let expr = parse_expr(tokens, index, 0);

            expect(tokens, index, Token::CloseParen)?;

            return expr;
        }

        Some(other_token) => bail!("Unexpected token {}", other_token.token),
        None => {
            bail!("Expected token, found the end of file")
        }
    }
}

fn list<T>(
    tokens: &Vec<TokenSpan>,
    index: &mut usize,
    cb: &dyn Fn(&Vec<TokenSpan>, &mut usize) -> Result<T>,
) -> Result<Vec<T>> {
    let mut statements = vec![];
    loop {
        match tokens.get(*index).map(|t| &t.token) {
            Some(Token::Eof | Token::NewLine) => break,
            _ => {
                match cb(tokens, index) {
                    Ok(decl) => {
                        statements.push(decl);
                    }
                    Err(e) => {
                        if !e.to_string().is_empty() {
                            bail!(e);
                        }
                        // bail!("Expected expression at {}", *index)
                        break;
                    }
                }

                match tokens.get(*index).map(|t| &t.token) {
                    Some(Token::Comma) => {
                        *index += 1;
                    }
                    Some(Token::NewLine | Token::Eof) => break,
                    _ => {
                        break;
                    }
                }
            }
        }
    }

    Ok(statements)
}

fn expect(tokens: &Vec<TokenSpan>, index: &mut usize, expected: Token) -> Result<()> {
    match tokens.get(*index).map(|t| &t.token) {
        Some(token) if token == &expected => {
            *index += 1;
            Ok(())
        }
        x => bail!("Expected {}, got {}", expected, x.unwrap_or(&Token::Eof)),
    }
}

fn expect_ident<'a>(tokens: &'a Vec<TokenSpan>, index: &mut usize) -> Result<&'a str> {
    match tokens.get(*index).map(|t| &t.token) {
        Some(Token::Ident(s)) => {
            *index += 1;
            Ok(s)
        }
        other_token => {
            bail!(
                "Expected identifier, got {}",
                other_token.unwrap_or(&Token::Eof)
            )
        }
    }
}

pub fn node_span_with(node: AstNode, line: usize, start: usize) -> AstNodeSpan {
    AstNodeSpan { node, line, start }
}

fn expect_keyword<'a>(
    tokens: &'a Vec<TokenSpan>,
    index: &mut usize,
    cb: &dyn Fn(&Keyword) -> bool,
) -> Result<String, Option<&'a Token>> {
    match tokens.get(*index).map(|t| &t.token) {
        Some(Token::Kwd(x)) if cb(x) => {
            *index += 1;
            Ok(x.to_string())
        }
        other_token => Err(other_token),
    }
}

fn expect_type(tokens: &Vec<TokenSpan>, index: &mut usize) -> Result<String> {
    fn type_validator(tok: &Keyword) -> bool {
        [
            Keyword::Int,
            Keyword::Float,
            Keyword::String,
            Keyword::PInt32,
        ]
        .contains(tok)
    }
    expect_keyword(tokens, index, &type_validator)
        .map_err(|other_type| anyhow!("Unknown type {}", other_type.unwrap_or(&Token::Eof)))
}

fn skip_new_lines<'a>(tokens: &'a Vec<TokenSpan>, index: &mut usize) {
    while let Some(Token::NewLine) = tokens.get(*index).map(|t| &t.token) {
        *index += 1;
    }
}

fn get_op_prec(op: &Token) -> Option<(u8, Assoc)> {
    // https://en.cppreference.com/w/cpp/language/operator_precedence
    macro_rules! c_prec {
        ($v: expr) => {
            18 - $v
        };
    }
    Some(match op {
        Token::OpenParen => (c_prec!(2), Assoc::Left),
        Token::OpenBracket => (c_prec!(2), Assoc::Left),
        Token::BitNot => (c_prec!(3), Assoc::Right),
        Token::Not => (c_prec!(3), Assoc::Right),
        Token::Mul => (c_prec!(5), Assoc::Left),
        Token::Div => (c_prec!(5), Assoc::Left),
        Token::Add => (c_prec!(6), Assoc::Left),
        Token::Sub => (c_prec!(6), Assoc::Left),
        Token::Less => (c_prec!(9), Assoc::Left),
        Token::LessEqual => (c_prec!(9), Assoc::Left),
        Token::Greater => (c_prec!(9), Assoc::Left),
        Token::GreaterEqual => (c_prec!(9), Assoc::Left),
        Token::EqualEqual => (c_prec!(10), Assoc::Left),
        Token::NotEqual => (c_prec!(10), Assoc::Left),
        Token::BitAnd => (c_prec!(11), Assoc::Left),
        Token::BitXor => (c_prec!(12), Assoc::Left),
        Token::BitOr => (c_prec!(13), Assoc::Left),
        Token::And => (c_prec!(14), Assoc::Left),
        Token::Or => (c_prec!(15), Assoc::Left),
        Token::Comma => (c_prec!(16), Assoc::Left),
        Token::Equal => (c_prec!(17), Assoc::Right),
        Token::AddEqual => (c_prec!(17), Assoc::Right),
        Token::SubEqual => (c_prec!(17), Assoc::Right),
        Token::MulEqual => (c_prec!(17), Assoc::Right),
        Token::DivEqual => (c_prec!(17), Assoc::Right),

        Token::BitAndEqual => (c_prec!(17), Assoc::Right),
        Token::BitOrEqual => (c_prec!(17), Assoc::Right),
        Token::BitXorEqual => (c_prec!(17), Assoc::Right),
        _ => return None,
    })
}

impl Display for AstNodeSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node)
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            AstNode::Root(children) => {
                for child in children {
                    write!(f, "{}\n", child.node)?;
                }
                Ok(())
            }
            AstNode::NodeAssignmentStmt(node) => write!(f, "{}", node),
            AstNode::NodeIndexExpression(node) => write!(f, "{}", node),
            AstNode::NodeCallExpression(node) => write!(f, "{}", node),
            AstNode::NodeVarDeclStml(node) => write!(f, "{}", node),
            AstNode::NodeConstDeclStml(node) => write!(f, "{}", node),
            AstNode::NodeFuncDeclStml(node) => write!(f, "{}", node),
            AstNode::NodeBinary(node) => write!(f, "{}", node),
            AstNode::NodeUnary(node) => write!(f, "{}{}", node.op, node.node),
            AstNode::NodeIntLiteral(i) => write!(f, "{}", i),
            AstNode::NodeLabelLiteral(s) => write!(f, "@{}", s),
            AstNode::NodeStringLiteral(s) => {
                write!(f, "\"")?;
                for c in s {
                    write!(f, "{}", *c as char)?;
                }
                write!(f, "\"")
            }
            AstNode::NodeReturnStmt(node) => match &node.expr {
                Some(expr) => write!(f, "return {}", expr),
                None => write!(f, "return"),
            },
            AstNode::NodeDefaultReturnStmt => {
                write!(f, "return")
            }
            AstNode::NodeFloatLiteral(fl) => {
                if fl.fract() == 0.0 {
                    write!(f, "{:.1}", fl)
                } else {
                    write!(f, "{}", fl)
                }
            }
            AstNode::NodeIdentifier(s) => write!(f, "{}", s),
            AstNode::NodeList(nodes) => {
                for node in nodes {
                    write!(f, "{}, ", node)?;
                }
                Ok(())
            }
            AstNode::NodeIfStatement(node) => {
                writeln!(f, "if {}", node.cond)?;
                writeln!(f, "then")?;
                for node in &node.then_block {
                    write!(f, "{}, ", node)?;
                }
                if let Some(ref nodes) = node.else_block {
                    for node in nodes {
                        write!(f, "{}, ", node)?;
                    }
                }
                writeln!(f, "end")
            }
            AstNode::NodeWhileStatement(node) => {
                writeln!(f, "while {}", node.cond)?;
                for node in &node.body {
                    write!(f, "{}, ", node)?;
                }
                writeln!(f, "end")
            }
            AstNode::NodeBreakStatement => {
                writeln!(f, "break")
            }
            AstNode::NodeContinueStatement => {
                writeln!(f, "continue")
            }
        }
    }
}

impl AstNode {
    pub fn is_statement(&self) -> bool {
        match self {
            // statements
            AstNode::Root(_)
            | AstNode::NodeList(_)
            | AstNode::NodeAssignmentStmt(_)
            | AstNode::NodeVarDeclStml(_)
            | AstNode::NodeConstDeclStml(_)
            | AstNode::NodeFuncDeclStml(_)
            | AstNode::NodeReturnStmt(_)
            | AstNode::NodeDefaultReturnStmt
            | AstNode::NodeIfStatement(_)
            | AstNode::NodeWhileStatement(_)
            | AstNode::NodeBreakStatement
            | AstNode::NodeContinueStatement => true,

            // expressions
            AstNode::NodeIdentifier(_)
            | AstNode::NodeIndexExpression(_)
            | AstNode::NodeCallExpression(_)
            | AstNode::NodeUnary(_)
            | AstNode::NodeBinary(_)
            | AstNode::NodeIntLiteral(_)
            | AstNode::NodeFloatLiteral(_)
            | AstNode::NodeLabelLiteral(_)
            | AstNode::NodeStringLiteral(_) => false,
        }
    }
}

#[repr(C)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ArgType {
    Void = 0,
    Int = 1,
    Float = 2,
    String = 3,
    PInt32 = 10,
}

impl Into<u8> for ArgType {
    fn into(self) -> u8 {
        self as u8
    }
}

impl Display for ArgType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            ArgType::Void => write!(f, "void"),
            ArgType::Int => write!(f, "int"),
            ArgType::Float => write!(f, "float"),
            ArgType::String => write!(f, "string"),
            ArgType::PInt32 => write!(f, "pint32"),
        }
    }
}

impl From<&String> for ArgType {
    fn from(s: &String) -> Self {
        match s.to_ascii_lowercase().as_str() {
            "int" => ArgType::Int,
            "float" => ArgType::Float,
            "string" | "gxt_key" | "zone_key" => ArgType::String,
            "pint32" | "pint" => ArgType::PInt32,
            _ => ArgType::Int,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeBinary {
    pub lhs: AstNodeSpan,
    pub rhs: AstNodeSpan,
    pub op: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeReturnStmt {
    pub expr: Option<AstNodeSpan>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeUnary {
    pub node: AstNodeSpan,
    pub op: Token,
}

impl Display for NodeUnary {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.op, self.node.node)
    }
}

impl Display for NodeBinary {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} {} {}", self.lhs.node, self.op, self.rhs.node)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeIndexExpression {
    pub name: AstNodeSpan,
    pub index: AstNodeSpan,
}

impl Display for NodeIndexExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}[{}]", self.name.node, self.index.node)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeCallExpression {
    pub name: AstNodeSpan,
    pub args: Option<AstNodeSpan>,
}

impl Display for NodeCallExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.name,
            match &self.args {
                Some(args) => args.node.to_string(),
                None => "".to_string(),
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeAssignmentStmt {
    pub lhs: AstNodeSpan,
    pub rhs: AstNodeSpan,
}

impl Display for NodeAssignmentStmt {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} = {}", self.lhs, self.rhs)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeVarDeclStml {
    pub names: Vec<AstNodeSpan>,
    pub ty: ArgType,
    pub expr: Option<AstNodeSpan>,
}

impl Display for NodeVarDeclStml {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let names = self
            .names
            .iter()
            .map(|x| x.node.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        match &self.expr {
            Some(expr) => write!(f, "{} {} = {}", self.ty, names, expr),
            None => write!(f, "{} {}", self.ty, names),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeConstDeclStml {
    pub names: Vec<AstNodeSpan>,
    pub expr: AstNodeSpan,
}

impl Display for NodeConstDeclStml {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let names = self
            .names
            .iter()
            .map(|x| x.node.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "const {} = {}", names, self.expr)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeFuncDeclStml {
    pub name: String,
    pub body: Vec<AstNodeSpan>,
    pub args: Vec<(String, AstNodeSpan)>,
    pub return_types: Vec<String>,
    pub is_optional: bool,
    pub is_logical: bool,
    pub is_exported: bool,
}

impl Display for NodeFuncDeclStml {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        writeln!(f, "function {}", self.name)?;
        for stmt in &self.body {
            writeln!(f, "{}", stmt.node)?;
        }
        writeln!(f, "end")?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeIfStatement {
    pub cond: AstNodeSpan,
    pub then_block: Vec<AstNodeSpan>,
    pub else_block: Option<Vec<AstNodeSpan>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeWhileStatement {
    pub cond: AstNodeSpan,
    pub body: Vec<AstNodeSpan>,
}

#[derive(PartialEq)]
enum Assoc {
    Left,
    Right,
}
