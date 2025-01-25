use crate::{common_ffi::pchar_to_str, r#impl::run};
use anyhow::anyhow;
use libc::c_char;
pub type PChar = *const c_char;

#[no_mangle]
pub unsafe extern "C" fn compile(
    inputfile: PChar,
    outputfile: PChar,
    definitions_file: PChar,
    out: *mut PChar,
) -> bool {
    boolclosure! {{
        let inputfile = pchar_to_str(inputfile)?;
        let outputfile = pchar_to_str(outputfile)?;
        let definitions_file = pchar_to_str(definitions_file)?;
        let definitions = std::fs::read_to_string(definitions_file).ok()?;
        match run(inputfile, definitions)
            .and_then(|buf| std::fs::write(outputfile, buf).map_err(|e| anyhow!("{}", e))) {
            Ok(_) => {
                Some(())
            }
            Err(e) => {
                *out = std::ffi::CString::new(e.to_string()).unwrap().into_raw();
                None
            }
        }
    }}
}
