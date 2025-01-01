use chrono::prelude::*;

use super::{
    collect_params, eat_variable, get_pointer_to_variable, skip_end_of_args, store_params,
    OpcodeResult, Scope, Script, SCRIPT_PARAMS, STACK,
};
use anyhow::{bail, Result};

// PRINT_HELP
pub fn opcode_03e5(script: &mut Script, vm: &mut crate::VM) -> Result<OpcodeResult> {
    collect_params(script, 1)?;
    (vm.print)(format!("print_help: {}", unsafe { param![0] }));
    Ok(OpcodeResult::Continue)
}

pub fn opcode_004e(_script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    Ok(OpcodeResult::Stop)
}

pub fn opcode_0006(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    let ptr = get_pointer_to_variable(script)?;
    collect_params(script, 1)?;
    unsafe { *ptr = param![0] };
    Ok(OpcodeResult::Continue)
}

pub fn opcode_00aa(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    collect_params(script, 1)?;
    unsafe {
        SCRIPT_PARAMS[0] = 0;
        SCRIPT_PARAMS[1] = 0;
        SCRIPT_PARAMS[2] = 0;
    }
    store_params(script, 3)?;
    Ok(OpcodeResult::Continue)
}

pub fn opcode_00bf(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    unsafe {
        let now = Local::now();
        SCRIPT_PARAMS[0] = now.hour() as _;
        SCRIPT_PARAMS[1] = now.minute() as _;
    }
    store_params(script, 2)?;
    Ok(OpcodeResult::Continue)
}

// CSET_LVAR_INT_TO_LVAR_FLOAT
pub fn opcode_0092(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    let ptr = get_pointer_to_variable(script)?;

    collect_params(script, 1)?;

    let f: f32 = unsafe { param!(0) as _ };
    unsafe { *ptr = std::mem::transmute(f) };

    Ok(OpcodeResult::Continue)
}

// CSET_LVAR_FLOAT_TO_LVAR_INT
pub fn opcode_0093(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    let ptr = get_pointer_to_variable(script)?;

    collect_params(script, 1)?;

    let f: f32 = unsafe { std::mem::transmute(param!(0)) };

    let i: i32 = f as _;
    unsafe { *ptr = i };

    Ok(OpcodeResult::Continue)
}

pub fn opcode_00ab(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    collect_params(script, 4)?;
    Ok(OpcodeResult::Continue)
}

pub fn opcode_0a8e(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    collect_params(script, 2)?;
    unsafe { SCRIPT_PARAMS[0] = SCRIPT_PARAMS[0] + SCRIPT_PARAMS[1] }
    store_params(script, 1)?;
    Ok(OpcodeResult::Continue)
}

pub fn opcode_0a8f(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    collect_params(script, 2)?;
    unsafe { SCRIPT_PARAMS[0] = SCRIPT_PARAMS[0] - SCRIPT_PARAMS[1] }
    store_params(script, 1)?;
    Ok(OpcodeResult::Continue)
}

pub fn opcode_0a90(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    collect_params(script, 2)?;
    unsafe { SCRIPT_PARAMS[0] = SCRIPT_PARAMS[0] * SCRIPT_PARAMS[1] }
    store_params(script, 1)?;
    Ok(OpcodeResult::Continue)
}

pub fn opcode_0a91(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    collect_params(script, 2)?;
    unsafe { SCRIPT_PARAMS[0] = SCRIPT_PARAMS[0] / SCRIPT_PARAMS[1] }
    store_params(script, 1)?;
    Ok(OpcodeResult::Continue)
}

pub fn opcode_0ab1(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    collect_params(script, 2)?;

    let label = unsafe { param![0] };
    let num_input_args = unsafe { param![1] };

    // collect function arguments
    collect_params(script, num_input_args as _)?;

    let mut scope = Scope::new(label.abs() as _);
    for i in 0..num_input_args as usize {
        scope.lvars[i] = unsafe { param![i] };
    }
    script.scopes.push(scope);

    Ok(OpcodeResult::Continue)
}

pub fn opcode_2002(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    collect_params(script, 1)?;

    let flag = unsafe { param![0] };

    // collect function results
    let mut results = vec![];
    while script.peek()? != 0 {
        collect_params(script, 1)?;
        results.push(unsafe { param![0] });
    }

    // go back to 0AB1
    if script.scopes.len() > 1 {
        // don't pop the main scope
        script.scopes.pop();
    }

    // store the results
    for result in results {
        unsafe {
            SCRIPT_PARAMS[0] = result;
        }
        store_params(script, 1)?;
    }

    skip_end_of_args(script)?;
    script.set_cond_result(flag != 0);
    Ok(OpcodeResult::Continue)
}

pub fn opcode_2003(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    // go back to 0AB1
    if script.scopes.len() > 1 {
        // don't pop the main scope
        script.scopes.pop();
    }
    // // skip the results
    while script.peek()? != 0 {
        eat_variable(script, 1)?
    }

    skip_end_of_args(script)?;
    script.set_cond_result(false);
    Ok(OpcodeResult::Continue)
}

pub fn opcode_2004(script: &mut Script, _vm: &mut crate::VM) -> Result<OpcodeResult> {
    collect_params(script, 1)?;
    unsafe {
        let n = SCRIPT_PARAMS[0];
        for i in 0..n {
            let Some(value) = STACK.pop() else {
                bail!("Stack is empty")
            };
            SCRIPT_PARAMS[i as usize] = value;
        }
        for i in 0..n {
            STACK.push(SCRIPT_PARAMS[i as usize]);
        }
    }
    Ok(OpcodeResult::Continue)
}
