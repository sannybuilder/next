use crate::{
    parser::ArgType,
    scope::{Scopes, VarType},
};
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

/// returns the variable that holds an offset to statically allocated memory block in the script where static variables keep their values
/// used in array costructs like &0(0@,1i)
pub fn get_persistent_storage_var(scopes: &mut Scopes) -> OpcodeArgument {
    let scope = scopes.get_current_scope();
    OpcodeArgument::LVAR(scope.get_persistent_storage_pointer(), ArgType::Int)
}

pub fn get_frame_pointer_var(scopes: &mut Scopes) -> OpcodeArgument {
    let scope = scopes.get_current_scope();
    OpcodeArgument::LVAR(scope.get_frame_pointer(), ArgType::Int)
}

pub fn get_storage_for_var(var_type: VarType, scopes: &mut Scopes) -> OpcodeArgument {
    match var_type {
        VarType::Persistent => get_persistent_storage_var(scopes),
        VarType::Frame => get_frame_pointer_var(scopes),
        _ => unreachable!("no other types here"),
    }
}

pub fn variable_to_argument(
    var_type: crate::scope::VarType,
    index: usize,
    ty: ArgType,
    scopes: &mut Scopes,
) -> OpcodeArgument {
    match var_type {
        VarType::Persistent | VarType::Frame => OpcodeArgument::ARRAY(Box::new((
            OpcodeArgument::GVAR(index, ty),
            get_storage_for_var(var_type, scopes),
            1,
        ))),
        VarType::Local => OpcodeArgument::LVAR(index, ty),
    }
}
