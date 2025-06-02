#[cfg(test)]
mod suite {
    use crate::{backends::Backend, compiler, toolchain};
    use anyhow::Result;
    use pretty_assertions::assert_eq;

    struct SimpleBackend {
        lines: Vec<String>,
    }

    impl Backend for SimpleBackend {
        fn process(&mut self, insts: Vec<compiler::Instruction>) -> Result<()> {
            for inst in insts {
                match inst {
                    compiler::Instruction::OpcodeInst(op) => self.lines.push(format!(
                        "{:04X}: {}",
                        op.id,
                        op.args
                            .iter()
                            .map(|x| format!("{}", x))
                            .collect::<Vec<_>>()
                            .join(" ")
                    )),
                    _ => {}
                }
            }
            Ok(())
        }
    }

    fn process(input: &str) -> Result<Vec<String>> {
        let mut backend = SimpleBackend { lines: vec![] };
        toolchain::run(input.as_bytes(), &mut backend).map(|_| backend.lines)
    }

    fn process_error(input: &str) -> String {
        let result = process(input);
        match result {
            Ok(_) => {
                println!("Line {} is not an error", input);
                assert!(false);
            }
            Err(_) => {}
        }

        result.unwrap_err().to_string()
    }

    #[test]
    fn test_err1() {
        assert_eq!(
            process_error("float pos[3] = "),
            "Unexpected token end of file at line 1 col 16"
        );
        assert_eq!(
            process_error("float pos[3] = 1.0, 2.0"),
            "Expected 3 values, got 2 at line 1"
        );
        assert_eq!(
            process_error("float pos[4] = get_char_coordinates(0)"),
            "Expected 4 values, got 3 at line 1"
        );
        assert_eq!(
            process_error("float pos[2] = get_char_coordinates(0)"),
            "Not enough variables to store the result of get_char_coordinates(0) at line 1"
        );

        assert_eq!(
            process_error("3 = 3"),
            "Invalid assignment target at line 1"
        );

        assert_eq!(
            process_error("int a, b\n a, b= 1"),
            "Expected 2 values, got 1 at line 2"
        );

        assert_eq!(
            process_error("int a\nint a"),
            "Can't declare 'a'. A variable with the same name already exists at line 2"
        );

        assert_eq!(
            process_error("int a[2]\nint a[2]"),
            "Can't declare 'a'. A variable with the same name already exists at line 2"
        );

        assert_eq!(process_error("int i_x = @label"), "Can't find label label");

        assert_eq!(
            process_error("int a, b = 1"),
            "Expected 2 values, got 1 at line 1"
        );

        assert_eq!(
            process_error("float f_x, f_y, f_z = get_char_coordinates(1), f_w = 1.0"),
            // "Expected a line break, got = at line 1 col 52"
            "Expected expression, found get_char_coordinates(1), f_w,  = 1.0 at line 1"
        );

        assert_eq!(
            process_error("int x = 1 + 2.0"),
            "Incompatible types: int(1) and float(2.0) at line 1"
        );

        assert_eq!(
            process_error("break"),
            "Break statement outside of loop at line 1"
        );
        assert_eq!(
            process_error("continue"),
            "Continue statement outside of loop at line 1"
        );
    }

    #[test]
    fn arrays() {
        assert_eq!(
            process_error("float pos[0]"),
            "Wrong array count 0 at line 1"
        );
        assert_eq!(
            process_error("float pos[~0]"),
            "Wrong array count -1 at line 1"
        );
        assert_eq!(
            process_error("float pos[3&0]"),
            "Wrong array count 0 at line 1"
        );
        assert_eq!(
            process_error("float pos[0 | 0]"),
            "Wrong array count 0 at line 1"
        );
        assert_eq!(
            process_error("float pos[3 ^ 3]"),
            "Wrong array count 0 at line 1"
        );
        assert_eq!(
            process_error("int a[3]\na[-1] = 0"),
            "Negative index -1 at line 2"
        );
        assert_eq!(
            process_error("int a[3]\nwait(a[-1])"),
            "Negative index -1 at line 2"
        );
        assert_eq!(
            process_error("int a[3]\nwait(a[3])"),
            "Out of bound index 3 in array of size 3 at line 2"
        );
        assert_eq!(
            process_error("int a\nwait(a[0])"),
            "a is not an array at line 2"
        );
        assert_eq!(
            process_error("int a\na[0]=1"),
            "a is not an array at line 2"
        );
        assert_eq!(
            process_error("int a\n3[0]=1"),
            "3 is not an array at line 2"
        );
        assert_eq!(
            process_error("int a\n3.0[0]=1.0"),
            "3.0 is not an array at line 2"
        );
        assert_eq!(
            process_error("int a\na[0][0]=1"),
            "a[0] is not an array at line 2"
        );
        assert_eq!(
            process_error("int a\nwait(3[0])"),
            "3 is not an array at line 2"
        );

        assert_eq!(
            process_error("float pos[-3/2]"),
            "Can't evaluate const expression -3 / 2 at line 1"
        );

        assert_eq!(
            process_error("int pos[1.0]"),
            "Expected integer number, got 1.0 at line 1"
        );

        assert_eq!(
            process_error("float f\nint pos[2]\npos[f]=1"),
            "Unsupported index f with type float at line 3"
        );

        assert_eq!(
            process_error("int a[3]\na[1.0]=1"),
            "Unsupported index 1.0 with type float at line 2"
        );

        assert_eq!(
            process_error("export function foo\nint a[16383]\nint z\nend"),
            "Run out of memory. Number of allocated variables exceeds 16383 at line 3"
        );
    }

    #[test]
    fn functions() {
        assert_eq!(
            process_error("wait()"),
            "Function 'wait' expected 1 arguments, got 0 at line 1"
        );
        assert_eq!(
            process_error("function foo\n"),
            "Expected end, got end of file at line 2 col 1"
        );
        assert_eq!(
            process_error("int x = wait(0)"),
            "Function 'wait' does not return anything at line 1"
        );
        assert_eq!(
            process_error("int x = foo()"),
            "Unknown function 'foo' at line 1"
        );

        assert_eq!(process_error("foo()"), "Unknown function 'foo' at line 1");

        assert_eq!(
            process_error("function foo(a: int, A: int)\nend"),
            "Can't declare 'A'. A variable with the same name already exists at line 1"
        );
        assert_eq!(
            process_error("function foo:\nend"),
            "Expected a return type at line 1 col 14"
        );

        assert_eq!(
            process_error("function foo\nend\nfunction foo\nend"),
            "Can't declare 'foo'. A function with the same name already exists at line 3"
        );
        assert_eq!(
            process_error("int foo\nfunction foo\nend"),
            "Can't declare 'foo'. A function with the same name already exists at line 1"
        );
        assert_eq!(
            process_error("return wait(0)"),
            "Return statement outside of function at line 1"
        );
        assert_eq!(
            process_error("function foo\nreturn 1 + wait(0)\nend"),
            "A function should have 1 output param to be used in an expression. 'wait' has 0 at line 2"
        );
        assert_eq!(
            process_error("function foo\nreturn 1 + wait\nend"),
            "Unknown name wait at line 2. Did you mean wait()?"
        );
        assert_eq!(
            process_error("function foo\nreturn 1 +\nend"),
            "Expected expression at line 3 col 1"
        );

        assert_eq!(
            process_error("function foo:int\nreturn 1.0\nend"),
            "Incompatible types: float(1.0) and int at line 2"
        );

        assert_eq!(
            process_error("function foo:int\nreturn\nend"),
            "Function 'foo' returns 1 values, but 0 found at line 2"
        );

        assert_eq!(
            process_error("function foo\nreturn 1\nend"),
            "Function 'foo' returns 0 values, but 1 found at line 2"
        );

        assert_eq!(
            process_error("function foo(i:int)\nend\nfoo(1.0)"),
            "Incompatible types: float(1.0) and int at line 3"
        );

        assert_eq!(
            process_error("function foo: optional int,int\nreturn 1\nend"),
            "Function 'foo' returns 2 values, but 1 found at line 2"
        );

        assert_eq!(
            process_error("start_new_script()"),
            "Variadic function 'start_new_script' expected at least 1 arguments, got 0 at line 1"
        );

        assert_eq!(
            process_error("start_new_script(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33)"),
            "Too many arguments for variadic function 'start_new_script', expected up to 32 arguments, got 33 at line 1"
        );

        assert_eq!(
            process_error("function foo(i:unknown)\nend"),
            "Unknown type unknown at line 1 col 16"
        );

        assert_eq!(
            process_error("function foo(i:5)\nend"),
            "Unknown type 5 at line 1 col 16"
        );

        assert_eq!(
            process_error("function foo(5:5)\nend"),
            "Expected identifier, got 5 at line 1 col 14"
        );

        assert_eq!(
            process_error("function foo:unknown\nend"),
            "Unknown type unknown at line 1 col 14"
        );
        assert_eq!(
            process_error("function foo:42\nend"),
            "Unknown type 42 at line 1 col 14"
        );

        // function x is hoisted and then become available in foo, creating a collision with foo's x
        assert_eq!(
            process_error("function foo\nfunction x\nend\nend\nfunction x\nend"),
            "Can't declare 'x'. A function with the same name already exists at line 2"
        )
    }

    #[test]
    fn code_with_comments() {
        assert_eq!(
            process_error("/*\n*/\n3=3"),
            "Invalid assignment target at line 3"
        );

        assert_eq!(
            process_error("//test\n3=3"),
            "Invalid assignment target at line 2"
        );
    }

    #[test]
    fn if_statements() {
        assert_eq!(
            process_error("if wait(0)\nthen\nend"),
            "Expected logical expression, found wait(0) at line 1"
        );
    }

    #[test]
    fn unary() {
        assert_eq!(
            process_error("int x = ~1.0"),
            "Expected integer type, got float at line 1"
        );
        assert_eq!(
            process_error("float a\nint x = ~a"),
            "Expected integer type, got float at line 2"
        );
        assert_eq!(
            process_error("float x = ~1"),
            "Incompatible types: float(x) and int(~1) at line 1"
        );
    }

    #[test]
    fn types() {
        assert_eq!(
            process_error("int x = 1.0"),
            "Incompatible types: int(x) and float(1.0) at line 1"
        );
        assert_eq!(
            process_error("int x\nfloat y\nx, y = 1, 2"),
            "Incompatible types: float(y) and int(2) at line 3"
        );
        assert_eq!(
            process_error("int pos[3] = get_char_coordinates(0)"),
            "Incompatible types: int(pos[0]) and float(get_char_coordinates(0)) at line 1"
        );
        assert_eq!(
            process_error("pint x = 1.0"),
            "Incompatible types: pint32(x) and float(1.0) at line 1"
        );
    }

    #[test]
    fn constants() {
        assert_eq!(
            process_error("int a\nconst a=1"),
            "Can't declare 'a'. A variable with the same name already exists at line 2"
        );
        assert_eq!(
            process_error("function a\nend\nconst a=1"),
            "Can't declare 'a'. A function with the same name already exists at line 3"
        );
        assert_eq!(
            process_error("const a=1\nconst a=1"),
            "Can't declare 'a'. A constant with the same name already exists at line 2"
        );
        assert_eq!(
            process_error("const a=1\nfunction foo\nconst a=1\nend"),
            "Can't declare 'a'. A constant with the same name already exists at line 3"
        );

        assert_eq!(
            process_error("const true=3"),
            "Can't declare 'true'. A constant with the same name already exists at line 1"
        );

        assert_eq!(
            process_error("const false=3"),
            "Can't declare 'false'. A constant with the same name already exists at line 1"
        );

        assert_eq!(
            process_error("function foo\nconst true=3\nend"),
            "Can't declare 'true'. A constant with the same name already exists at line 2"
        );

        assert_eq!(
            process_error("function foo\nconst false=3\nend"),
            "Can't declare 'false'. A constant with the same name already exists at line 2"
        );
    }
}
