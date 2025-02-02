use anyhow::{bail, Result};

use crate::{argument::OpcodeArgument, parser::ArgType};
use std::collections::HashMap;

#[derive(PartialEq, Clone, Debug)]
pub enum ScopeType {
    Root,
    Function(String),
}

pub const MAX_LVARS: usize = 32;

#[derive(Default)]
pub struct Scopes {
    scopes: Vec<Scope>,
    label_counter: usize,
    number_of_persistent_variables: usize,
    exported_functions: Vec<FunctionExport>,
}

pub struct Scope {
    ty: ScopeType,
    variables: HashMap<String, Variable>,
    functions: HashMap<String, Function>,
    constants: HashMap<String, ConstantSpan>,
    pub free_local_variables: [bool; MAX_LVARS],
    number_of_frame_variables: usize,
    total_number_of_frame_variables: usize,
    number_of_lvars: usize,
    loops: Vec<(String, String)>,
    line: usize,
}

#[derive(Clone)]
struct ConstantSpan {
    symbol: OpcodeArgument,
    line: usize,
}

pub struct FunctionExport {
    pub name: String,
    pub definition: Function,
}

/*
        memory model

                    HEAP                                        RUNTIME
    |------------------|-----------------|                |-----------------------|
    | persistent       | frame variables |                | 0..31 local variables |
    | variables (2)    | (3)             |                | in each function (1)  |
    |------------------|-----------------|                |-----------------------|

    - (1) local variables provided by SCM runtime (32 max). Used as function arguments, or array indexes. reused in each scope.
            two pointers (0@, 1@) are special variables used by the compiler. can not be reused.
    - (2) persistent variables declared in global scope with a unique index that can not be reused.
    - (3) frame variables declared in function scope, index is relative to frame pointer, can be reused in different functions
*/

impl Scope {
    fn new(ty: ScopeType, line: usize) -> Self {
        let mut variables = HashMap::new();
        let mut constants = HashMap::new();

        // built-in timers
        variables.insert(
            "timera".to_string(),
            Variable {
                count: 1,
                index: 32,
                ty: ArgType::Int,
                var_type: VarType::Local,
            },
        );
        variables.insert(
            "timerb".to_string(),
            Variable {
                count: 1,
                index: 33,
                ty: ArgType::Int,
                var_type: VarType::Local,
            },
        );

        constants.insert(
            "true".to_string(),
            ConstantSpan {
                line,
                symbol: OpcodeArgument::INT(1),
            },
        );

        constants.insert(
            "false".to_string(),
            ConstantSpan {
                line,
                symbol: OpcodeArgument::INT(0),
            },
        );

        Self {
            ty,
            variables,
            constants,
            functions: HashMap::new(),
            number_of_lvars: 0,
            number_of_frame_variables: 0,
            total_number_of_frame_variables: 0,
            free_local_variables: [true; MAX_LVARS],
            loops: Vec::new(),
            line,
        }
    }

    pub fn is_root_scope(&self) -> bool {
        self.ty == ScopeType::Root
    }

    pub fn is_function_scope(&self) -> bool {
        matches!(self.ty, ScopeType::Function(_))
    }

    pub fn get_frame_pointer(&self) -> usize {
        self.number_of_lvars - 1
    }

    pub fn get_persistent_storage_pointer(&self) -> usize {
        self.number_of_lvars - 2
    }

    pub fn reserve_space_for_pointers(&mut self) {
        // one variable for persistent storage pointer and one for frame pointer
        self.number_of_lvars += 2;
    }

    pub fn get_type(&self) -> &ScopeType {
        &self.ty
    }

    pub fn find_var(&self, name: &str) -> Option<&Variable> {
        self.variables.get(&name.to_ascii_lowercase())
    }

    pub fn find_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(&name.to_ascii_lowercase())
    }

    pub fn find_constant(&self, name: &str) -> Option<&OpcodeArgument> {
        self.constants
            .get(&name.to_ascii_lowercase())
            .map(|x| &x.symbol)
    }

    pub fn register_fn(&mut self, name: &str, f: Function, line: usize) -> Result<()> {
        self.assert_unique_name(name)?;
        let name_lower = name.to_ascii_lowercase();
        self.register_const(name, OpcodeArgument::LABEL(name_lower.clone()), line)?;
        self.functions.insert(name_lower, f);
        Ok(())
    }

    pub fn register_const(&mut self, name: &str, value: OpcodeArgument, line: usize) -> Result<()> {
        self.assert_unique_name(name)?;
        let name_lower = name.to_ascii_lowercase();
        self.constants.insert(
            name_lower,
            ConstantSpan {
                symbol: value,
                line,
            },
        );
        Ok(())
    }

    fn assert_unique_name(&self, name: &str) -> Result<()> {
        let name_lower = name.to_ascii_lowercase();
        if self.functions.contains_key(&name_lower) {
            bail!("Can't declare '{name}'. A function with the same name already exists")
        }
        if self.variables.contains_key(&name_lower) {
            bail!("Can't declare '{name}'. A variable with the same name already exists")
        };
        if self.constants.contains_key(&name_lower) {
            bail!("Can't declare '{name}'. A constant with the same name already exists")
        };

        Ok(())
    }

    pub fn push_loop_labels(&mut self, start_label: String, exit_label: String) {
        self.loops.push((start_label, exit_label));
    }

    pub fn pop_loop_labels(&mut self) {
        self.loops.pop();
    }

    pub fn get_loop_labels(&self) -> Option<&(String, String)> {
        self.loops.last()
    }

    pub fn get_frame_size(&self) -> usize {
        self.total_number_of_frame_variables
    }

    pub fn set_frame_size(&mut self, value: usize) {
        self.total_number_of_frame_variables = value;
    }
}

pub fn get_number_of_slots(ty: &ArgType) -> usize {
    match ty {
        ArgType::String => 4,
        ArgType::Void => 0,
        _ => 1,
    }
}

impl Scopes {
    pub fn new() -> Self {
        let scopes = Scopes::default();
        scopes
    }
    pub fn get_current_scope<'a>(&'a mut self) -> &'a mut Scope {
        self.scopes.last_mut().unwrap()
    }
    pub fn enter(&mut self, ty: ScopeType, line: usize) {
        let mut scope = Scope::new(ty.clone(), line);
        if ty == ScopeType::Root {
            let library = somersault_sbl::parse().unwrap();

            for e in library.extensions {
                for c in e.commands {
                    scope.functions.insert(
                        c.name.to_ascii_lowercase(),
                        Function {
                            op: c.id,
                            input_count: c.input.len(),
                            output_count: c.output.len(),
                            is_variadic: c
                                .input
                                .iter()
                                .chain(c.output.iter())
                                .any(|param| param.r#type.eq_ignore_ascii_case("arguments")),
                            input: {
                                let mut array = [ArgType::Void; MAX_LVARS];
                                for (i, elem) in c.input.iter().enumerate() {
                                    array[i] = (&elem.r#type).into();
                                }
                                array
                            },
                            output: {
                                let mut array = [ArgType::Void; MAX_LVARS];
                                for (i, elem) in c.output.iter().enumerate() {
                                    array[i] = (&elem.r#type).into();
                                }
                                array
                            },
                            is_optional: false,
                            output_first: false,
                            is_logical: c.attrs.is_condition,
                        },
                    );
                }
            }
        }
        if !self.scopes.is_empty() {
            self.inherit(&mut scope)
        }
        self.scopes.push(scope);
    }

    pub fn exit(&mut self) {
        self.scopes.pop();
    }

    fn inherit(&mut self, scope: &mut Scope) {
        let top = self.get_current_scope();
        scope.functions = top.functions.clone();

        // only inherit constants defined before the scope
        let mut map = HashMap::new();
        for (key, value) in top
            .constants
            .iter()
            .filter(|(_, value)| value.line < scope.line)
        {
            map.insert(key.clone(), value.clone());
        }
        scope.constants = map;

        //todo: global variables?
    }

    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty()
    }

    pub fn get_function_signature(&mut self, name: &str) -> Option<Function> {
        match name.to_ascii_lowercase().as_str() {
            "int" => Some(Function {
                op: 0x0093,
                input_count: 1,
                output_count: 1,
                output: {
                    let mut array = [ArgType::Void; MAX_LVARS];
                    array[0] = ArgType::Int;
                    array
                },
                input: {
                    let mut array = [ArgType::Void; MAX_LVARS];
                    array[0] = ArgType::Float;
                    array
                },
                is_variadic: false,
                is_optional: false,
                output_first: true,
                is_logical: false,
            }),
            "float" => Some(Function {
                op: 0x0092,
                input_count: 1,
                output_count: 1,
                output: {
                    let mut array = [ArgType::Void; MAX_LVARS];
                    array[0] = ArgType::Float;
                    array
                },
                input: {
                    let mut array = [ArgType::Void; MAX_LVARS];
                    array[0] = ArgType::Int;
                    array
                },
                is_variadic: false,
                is_optional: false,
                output_first: true,
                is_logical: false,
            }),
            _ => self.get_current_scope().find_function(name).cloned(),
        }
    }

    pub fn get_persistent_storage_size(&self) -> usize {
        self.number_of_persistent_variables * 4
    }

    pub fn get_frame_storage_size(&self) -> usize {
        256
    }

    pub fn unique_label(&mut self) -> String {
        self.label_counter += 1;
        format!("$internal.{}", self.label_counter)
    }

    pub fn frame_storage_label(&mut self) -> String {
        format!("$internal.storage.frame")
    }

    pub fn function_inner_label(&mut self, name: impl std::fmt::Display) -> String {
        format!("$internal.fn.{}", name)
    }

    pub fn allocate_local_var(&mut self, ty: ArgType, count: usize) -> Result<usize> {
        let scope = self.get_current_scope();
        let slots = get_number_of_slots(&ty) * count;
        for i in scope.number_of_lvars..MAX_LVARS {
            if scope.free_local_variables[i] {
                let mut free = true;
                if i + slots > MAX_LVARS {
                    break;
                }
                for j in 0..slots {
                    if !scope.free_local_variables[i + j] {
                        free = false;
                        break;
                    }
                }
                if free {
                    for j in 0..slots {
                        scope.free_local_variables[i + j] = false;
                    }
                    return Ok(i);
                }
            }
        }
        bail!("Run out of local variables");
    }

    pub fn deallocate_local_var(&mut self, index: usize) -> Result<()> {
        let scope = self.get_current_scope();
        if index >= scope.number_of_lvars {
            if index >= MAX_LVARS {
                bail!("Attempt to deallocate an invalid local variable {index}")
            }
            scope.free_local_variables[index] = true;
        }
        Ok(())
    }

    pub fn register_var(&mut self, name: String, ty: ArgType, var_type: VarType) -> Result<()> {
        self.create_variable(name, ty, 1, var_type)
    }

    pub fn register_array(
        &mut self,
        name: String,
        ty: ArgType,
        count: usize,
        var_type: VarType,
    ) -> Result<()> {
        self.create_variable(name, ty, count, var_type)
    }

    fn create_variable(
        &mut self,
        name: String,
        ty: ArgType,
        count: usize,
        var_type: VarType,
    ) -> Result<()> {
        let index = match var_type {
            VarType::Persistent => {
                let index = self.number_of_persistent_variables;
                let slots = get_number_of_slots(&ty) * count;
                self.number_of_persistent_variables += slots;
                index
            }
            VarType::Frame => {
                let scope = self.get_current_scope();
                let index = scope.number_of_frame_variables;
                let slots = get_number_of_slots(&ty) * count;
                scope.number_of_frame_variables += slots;
                index
            }
            VarType::Local => {
                let index = self.allocate_local_var(ty, count)?;
                let slots = get_number_of_slots(&ty) * count;
                let scope = self.get_current_scope();
                scope.number_of_lvars += slots;
                index
            }
        };
        let scope = self.get_current_scope();
        scope.assert_unique_name(&name)?;
        scope.variables.insert(
            name.to_ascii_lowercase(),
            Variable {
                ty,
                index,
                var_type,
                count,
            },
        );
        Ok(())
    }

    pub fn export_function(&mut self, name: String, definition: Function) {
        self.exported_functions
            .push(FunctionExport { name, definition });
    }

    pub fn get_exported_functions(&self) -> &Vec<FunctionExport> {
        &self.exported_functions
    }

    pub fn calculate_export_section_size(&self) -> i32 {
        self.exported_functions.iter().fold(0, |size, f| {
            size + f.name.len()
                + 1
                + 4
                + 1
                + f.definition.input_count
                + 1
                + f.definition.output_count
                + 1
                + 4
        }) as i32
    }
}

#[derive(PartialEq, Clone, Copy)]
pub enum VarType {
    Persistent, // global (static) variables
    Frame,      // local function variables
    Local,      // pre-allocated script variables only used for temp calculations
}

#[derive(PartialEq, Clone, Copy)]
pub struct Variable {
    pub ty: ArgType,
    pub index: usize,
    pub var_type: VarType,
    pub count: usize, // >1 array
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Function {
    pub op: u16,
    pub input_count: usize,
    pub output_count: usize,
    pub output: [ArgType; MAX_LVARS],
    pub input: [ArgType; MAX_LVARS],
    pub is_variadic: bool,
    pub is_optional: bool,
    pub is_logical: bool,
    pub output_first: bool,
}
