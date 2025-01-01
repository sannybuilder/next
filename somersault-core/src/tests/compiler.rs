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

    fn process_input(input: &str) -> Vec<String> {
        let result = process(input);
        if result.is_err() {
            println!("{:?} for input {}", result, input);
        }
        assert!(result.is_ok());
        let mut inst = result.unwrap();
        if inst.is_empty() {
            return vec![];
        };
        if inst.last().unwrap().starts_with("004E:") {
            inst.pop();
        }
        if !inst.is_empty() && inst[0].starts_with("0AC8:") {
            return inst.into_iter().skip(4).collect::<Vec<_>>();
        }

        inst
    }

    fn process_error(input: &str) -> String {
        let result = process(input);
        assert!(result.is_err());
        result.unwrap_err().to_string()
    }

    #[test]
    fn test_binary_int() {
        assert_eq!(process_input("int i_x"), vec![] as Vec<String>);

        assert_eq!(process_input("int i_x = 1"), vec!["0006: &0(0@,1i) 1"]);

        assert_eq!(process_input("int i_x = 1 + 2"), vec!["0006: &0(0@,1i) 3"]);

        assert_eq!(process_input("int i_x = 1 - 2"), vec!["0006: &0(0@,1i) -1"]);

        assert_eq!(
            process_input("int i_x = 2*5 - 1*5"),
            vec!["0006: &0(0@,1i) 5"]
        );

        assert_eq!(
            process_input("int i_x = 2*5 - 6"),
            vec!["0006: &0(0@,1i) 4"]
        );

        assert_eq!(
            process_input("int i_x = 6 - 2*5"),
            vec!["0006: &0(0@,1i) -4"]
        );

        assert_eq!(process_input("int i_x = 1 * 2"), vec!["0006: &0(0@,1i) 2"]);

        assert_eq!(
            process_input("int i_x = 1 / 2"),
            vec!["0A91: 1 2 &0(0@,1i)"]
        );

        assert_eq!(
            process_input("float f_x = 1.0 + 2.0"),
            vec!["0007: &0(0@,1f) 3.0"]
        );

        assert_eq!(
            process_input("float f_x = 1.0 - 2.0"),
            vec!["0007: &0(0@,1f) -1.0"]
        );

        assert_eq!(
            process_input("float f_x = 1.0 * 2.0"),
            vec!["0007: &0(0@,1f) 2.0"]
        );

        assert_eq!(
            process_input("float f_x = 1.0 / 2.0"),
            vec!["0007: &0(0@,1f) 0.5"]
        );

        assert_eq!(process_input("int i_x = 5"), vec!["0006: &0(0@,1i) 5"]);

        assert_eq!(
            process_input("float f_x = 5.0"),
            vec!["0007: &0(0@,1f) 5.0"]
        );

        assert_eq!(
            process_input("int i_x = 1 + 2 * 3 - 4 / 5"),
            vec!["0A91: 4 5 1@", "0A8F: 7 1@ &0(0@,1i)",]
        );

        assert_eq!(
            process_input("int i_x = (1 + 2) * 3"),
            vec!["0006: &0(0@,1i) 9"]
        );
    }

    #[test]
    fn test_binary_float() {
        assert_eq!(
            process_input("float a,b\na=a+b\na=a-b\na=a*b\na=a/b"),
            vec![
                "000B: &0(0@,1f) &4(0@,1f)",
                "000F: &0(0@,1f) &4(0@,1f)",
                "0013: &0(0@,1f) &4(0@,1f)",
                "0017: &0(0@,1f) &4(0@,1f)",
            ]
        );
        assert_eq!(
            process_input("float a,b\na+=b\na-=b\na*=b\na/=b"),
            vec![
                "000B: &0(0@,1f) &4(0@,1f)",
                "000F: &0(0@,1f) &4(0@,1f)",
                "0013: &0(0@,1f) &4(0@,1f)",
                "0017: &0(0@,1f) &4(0@,1f)",
            ]
        );
    }

    #[test]
    fn test_functions() {
        assert_eq!(
            process_input("function foo:int\nreturn 1+2\nend"),
            vec!["004E: ", "2002: 1 3"]
        );

        assert_eq!(
            process_error("function foo\nreturn nothing\nend"),
            "Unknown name nothing at line 2. Did you mean nothing()?"
        );

        assert_eq!(
            process_input("function foo:int\nreturn get_player_char(1) + get_player_char(2)\nend",),
            vec![
                "004E: ",
                "01F5: 1 1@",
                "01F5: 2 2@",
                "0A8E: 1@ 2@ 3@",
                "2002: 1 3@"
            ]
        );

        assert_eq!(
            process_input("function foo:float,float,float\nreturn get_char_coordinates(0)\nend"),
            vec!["004E: ", "00A0: 0 1@ 2@ 3@", "2002: 1 1@ 2@ 3@"]
        );

        assert_eq!(
            process_input("function foo:int,int\nreturn ((1+1),(2+(2)))\nend"),
            vec!["004E: ", "2002: 1 2 4"]
        );

        assert_eq!(
            process_input("function bar(i:int, b:float)\nend\nfunction foo:int,float\nreturn 1,2.0\nend\nbar(foo())"),
            vec!["0AB1: -43 1 0@ 1@ 2@", "0AB1: -40 3 0@ 1@ 2@", "004E: ", "2003: ", "2002: 1 1 2.0"]
        );

        assert_eq!(
            process_input("function foo:optional int\nreturn\nend"),
            vec!["004E: ", "2002: 0"]
        );

        assert_eq!(
            process_input("get_char_coordinates(0)"),
            vec!["00A0: 0 1@ 2@ 3@"]
        );

        assert_eq!(process_input("start_new_script(0)"), vec!["004F: 0"]);
        assert_eq!(
            process_input("start_new_script(0,  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)"),
            vec!["004F: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32"]
        );

        assert_eq!(process_input("function hour:int\nint a = 0\nreturn 5\nend\nfunction minute:int\nreturn 5\nend\nset_time_of_day( hour(), minute() )"), vec![
            "0AB1: -72 1 0@ 1@",
            "0AB1: -90 1 0@ 2@",
            "00C0: 1@ 2@",
            "004E: ",
            "0006: &0(0@,1i) 0",
            "2002: 1 5",
            "2002: 1 5",
        ]);

        assert_eq!(
            process_input("function x(a:int)\nz(a)\nz(a)\nfunction z(a:int):int\nend\nend"),
            vec![
                "004E: ",
                "0AB1: -43 2 0@ 1@ 2@",
                "0AB1: -43 2 0@ 1@ 2@",
                "2003: ",
                "2003: "
            ]
        );

        assert_eq!(
            process_input("function foo(a:int)\nint x = a*2 + 1\nint y = a*2 + 1\nend"),
            vec![
                "004E: ",
                "0A90: 1@ 2 2@",
                "0A8E: 2@ 1 &0(0@,1i)",
                "0A90: 1@ 2 2@",
                "0A8E: 2@ 1 &4(0@,1i)",
                "2003: "
            ]
        );

        assert_eq!(
            process_input("function x(a:int)\nwait(a+2)\nwait(a+2)\nend"),
            vec![
                "004E: ",
                "0A8E: 1@ 2 2@",
                "0001: 2@",
                "0A8E: 1@ 2 2@",
                "0001: 2@",
                "2003: "
            ]
        );
    }

    #[test]
    fn test_call() {
        assert_eq!(
            process_input("int i_x = get_player_char(0)"),
            vec!["01F5: 0 &0(0@,1i)"]
        );

        assert_eq!(
            process_input("print_big('test', 1000, 1)"),
            vec!["00BA: \"test\" 1000 1"]
        );

        assert_eq!(
            process_input("print_big('test', 1000, get_player_char(0))"),
            vec!["01F5: 0 1@", "00BA: \"test\" 1000 1@"]
        );
    }

    #[test]
    fn test_str() {
        assert_eq!(
            process_input("string s_x = 'hello'"),
            vec!["06D2: &0(0@,1v) \"hello\""]
        );
    }

    #[test]
    fn test_label() {
        assert_eq!(
            process_input("int i_x = foo\nfunction foo\nend"),
            vec!["0006: &0(0@,1i) -46", "004E: ", "2003: "]
        );
    }

    #[test]
    fn test_unary_not() {
        assert_eq!(process_input("int i_x = ~5"), vec!["0006: &0(0@,1i) -6"]);

        assert_eq!(
            process_input("int i_y\nint i_x = ~(i_y+1)"),
            vec!["0A8E: &0(0@,1i) 1 1@", "0B13: &4(0@,1i) 1@",]
        );

        assert_eq!(
            process_input("int i_y, i_z\nint i_x = ~i_y - i_z"),
            vec!["0B13: 1@ &0(0@,1i)", "0A8F: 1@ &4(0@,1i) &8(0@,1i)",]
        );

        assert_eq!(
            process_input("int i_x = 1 + ~2"),
            vec!["0006: &0(0@,1i) -2"]
        );
    }

    #[test]
    fn test_bitwise() {
        assert_eq!(process_input("int i_x = 1 & 2"), vec!["0006: &0(0@,1i) 0"]);

        assert_eq!(process_input("int i_x = 1 | 2"), vec!["0006: &0(0@,1i) 3"]);

        assert_eq!(process_input("int i_x = 1 ^ 2"), vec!["0006: &0(0@,1i) 3"]);
    }

    #[test]
    fn test_array() {
        assert_eq!(
            process_input("int x[3]\n x[0] = 1"),
            vec!["0006: &0(0@,1i) 1"]
        );

        assert_eq!(
            process_input("int x[3]\nx[0+1] = 1"),
            vec!["0006: &4(0@,1i) 1"]
        );

        assert_eq!(
            process_input("float f[3]\nf[0], f[1], f[2] = get_char_coordinates(0)"),
            vec!["00A0: 0 &0(0@,1f) &4(0@,1f) &8(0@,1f)",]
        );

        assert_eq!(
            process_input("int a = 1\nint f[2]\nint b = 2"),
            vec!["0006: &0(0@,1i) 1", "0006: &12(0@,1i) 2"]
        );

        assert_eq!(
            process_input("int a = 1\nstring f[2]\nint b = 2"),
            vec!["0006: &0(0@,1i) 1", "0006: &36(0@,1i) 2"]
        );

        assert_eq!(
            process_input("int x[3] = 1, 2, 3"),
            vec![
                "0006: &0(0@,1i) 1",
                "0006: &4(0@,1i) 2",
                "0006: &8(0@,1i) 3"
            ]
        );

        assert_eq!(
            process_input("int a=1\nint x[3] = 1, a, 3"),
            vec![
                "0006: &0(0@,1i) 1",
                "0006: &4(0@,1i) 1",
                "0006: &8(0@,1i) &0(0@,1i)",
                "0006: &12(0@,1i) 3",
            ]
        );

        assert_eq!(
            process_input("int a[3]\na[a[0]]=1"),
            vec!["0A8E: 0@ &0(0@,1i) 1@", "0006: &0(1@,1i) 1"]
        );

        assert_eq!(process_input("int x = ~-2"), vec!["0006: &0(0@,1i) 1"]);

        assert_eq!(
            process_input("int x[~-3]=1,2"), // ~-3=2
            vec!["0006: &0(0@,1i) 1", "0006: &4(0@,1i) 2"]
        );

        assert_eq!(
            process_input("export function foo\nint a[2]=1,2\nend"),
            vec!["0002: -35", "004E: ", "0006: 1@ 1", "0006: 2@ 2", "2003: "]
        );

        assert_eq!(
            process_input("export function foo\nint a[2]=1,2\nint z = 3\na[z]=50\nend"),
            vec![
                "0002: -35",
                "004E: ",
                "0006: 1@ 1",
                "0006: 2@ 2",
                "0006: 3@ 3",
                "0006: 1@(3@,1i) 50",
                "2003: "
            ]
        );

        // assert_eq!(
        //     process_input("f_z = get_player_pos(0)[2]"),
        //     vec![""]
        // )
    }

    #[test]
    fn test_assignment() {
        assert_eq!(
            process_input("int a, b\na, b = 1, 2"),
            vec!["0006: &0(0@,1i) 1", "0006: &4(0@,1i) 2",]
        );

        assert_eq!(
            process_input("int a\nfloat b\na, b = 1, 2.0"),
            vec!["0006: &0(0@,1i) 1", "0007: &4(0@,1f) 2.0",]
        );

        assert_eq!(
            process_input("int a, b\na, b = b, a"),
            vec![
                "0006: 1@ &4(0@,1i)",
                "0006: &4(0@,1i) &0(0@,1i)",
                "0006: &0(0@,1i) 1@",
            ]
        );

        assert_eq!(
            process_input("int a[2]\na[0], a[1] = a[1], a[0]"),
            vec![
                "0006: 1@ &4(0@,1i)",
                "0006: &4(0@,1i) &0(0@,1i)",
                "0006: &0(0@,1i) 1@",
            ]
        );

        assert_eq!(
            process_input("float x, y, z\nx, y, z = get_char_coordinates(1)"),
            vec!["00A0: 1 &0(0@,1f) &4(0@,1f) &8(0@,1f)",]
        );

        assert_eq!(
            process_input("int a,b\na=1\nb=a"),
            vec!["0006: &0(0@,1i) 1", "0006: &4(0@,1i) &0(0@,1i)"]
        );

        assert_eq!(
            process_input("int a,b,c=1,get_time_of_day()"),
            vec!["0006: &0(0@,1i) 1", "00BF: &4(0@,1i) &8(0@,1i)",]
        );

        assert_eq!(
            process_input("float a\na = 1.0 - a"),
            vec![
                "0007: 1@ &0(0@,1f)",
                "0007: &0(0@,1f) 1.0",
                "000F: &0(0@,1f) 1@"
            ]
        );

        assert_eq!(
            process_input("float a\na = a - a"),
            vec!["000F: &0(0@,1f) &0(0@,1f)"]
        );
    }

    #[test]
    fn test_multi_assignment() {
        assert_eq!(
            process_input("float f_x, f_y, f_z, f_w = get_char_coordinates(1), 1.0"),
            vec![
                "00A0: 1 &0(0@,1f) &4(0@,1f) &8(0@,1f)",
                "0007: &12(0@,1f) 1.0",
            ]
        );

        assert_eq!(
            process_input("float f[3], f_w = get_char_coordinates(1), 1.0"),
            vec![
                "00A0: 1 &0(0@,1f) &4(0@,1f) &8(0@,1f)",
                "0007: &12(0@,1f) 1.0",
            ]
        );
    }

    #[test]
    fn test_if_statement() {
        assert_eq!(
            process_input(
                "if is_player_playing(0) then\nwait(0)\nelse\nterminate_this_script()\nend"
            ),
            vec!["0256: 0", "004D: -22", "0001: 0", "0002: -24", "004E: ",]
        );

        assert_eq!(
            process_input(
                "if not is_player_playing(0) then\nwait(0)\nelse\nterminate_this_script()\nend"
            ),
            vec!["8256: 0", "004D: -22", "0001: 0", "0002: -24", "004E: ",]
        );

        assert_eq!(
            process_input("if 1 > 2 then\nend"),
            vec!["0019: 1 2", "004D: -13"]
        );

        assert_eq!(
            process_input("if 1 < 2 then\nend"),
            vec!["8029: 1 2", "004D: -13"]
        );

        assert_eq!(
            process_input("if 1 >= 2 then\nend"),
            vec!["0029: 1 2", "004D: -13"]
        );

        assert_eq!(
            process_input("if 1 <= 2 then\nend"),
            vec!["8019: 1 2", "004D: -13"]
        );

        assert_eq!(
            process_input("if 1 == 2 then\nend"),
            vec!["0039: 1 2", "004D: -13"]
        );

        assert_eq!(
            process_input("if 1 <> 2 then\nend"),
            vec!["8039: 1 2", "004D: -13"]
        );

        assert_eq!(
            process_input("if 1.0 > 2.0 then\nend"),
            vec!["0021: 1.0 2.0", "004D: -19"]
        );

        assert_eq!(
            process_input("if 1.0 < 2.0 then\nend"),
            vec!["8031: 1.0 2.0", "004D: -19"]
        );

        assert_eq!(
            process_input("if 1.0 >= 2.0 then\nend"),
            vec!["0031: 1.0 2.0", "004D: -19"]
        );

        assert_eq!(
            process_input("if 1.0 <= 2.0 then\nend"),
            vec!["8021: 1.0 2.0", "004D: -19"]
        );

        assert_eq!(
            process_input("if 1.0 == 2.0 then\nend"),
            vec!["0043: 1.0 2.0", "004D: -19"]
        );

        assert_eq!(
            process_input("if 1.0 <> 2.0 then\nend"),
            vec!["8043: 1.0 2.0", "004D: -19"]
        );
    }

    #[test]
    fn test_single_function() {
        assert_eq!(process_input("function foo\nend"), vec!["004E: ", "2003: "]);
    }

    #[test]
    fn test_types() {
        assert_eq!(
            process_input("int a\npint b=a"),
            vec!["0006: &4(0@,1i) &0(0@,1i)"]
        );
    }
    #[test]
    fn test_while_loop() {
        assert_eq!(
            process_input("while 1\nwait(0)\nend"),
            vec!["0000: ", "2704: 1", "004D: -24", "0001: 0", "0002: -2"]
        );
        assert_eq!(
            process_input("wait(0)\nwhile 1\nwait(0)\nend"),
            vec!["0001: 0", "2704: 1", "004D: -26", "0001: 0", "0002: -4"]
        );
        assert_eq!(
            process_input("while 1\ncontinue\nend"),
            vec!["0000: ", "2704: 1", "004D: -27", "0002: -2", "0002: -2"]
        );
        assert_eq!(
            process_input("while 1\nbreak\nend"),
            vec!["0000: ", "2704: 1", "004D: -27", "0002: -27", "0002: -2"]
        );

        assert_eq!(
            process_input("while 1\nwhile 1\nwait(0)\nend\nend"),
            vec![
                "0000: ",
                "2704: 1",
                "004D: -42",
                "2704: 1",
                "004D: -35",
                "0001: 0",
                "0002: -13",
                "0002: -2"
            ]
        );

        assert_eq!(
            process_input("while 1\nwhile 1\nbreak\nend\nend"),
            vec![
                "0000: ",
                "2704: 1",
                "004D: -45",
                "2704: 1",
                "004D: -38",
                "0002: -38",
                "0002: -13",
                "0002: -2"
            ]
        );

        assert_eq!(
            process_input("while 1\nwhile 1\ncontinue\nend\nend"),
            vec![
                "0000: ",
                "2704: 1",
                "004D: -45",
                "2704: 1",
                "004D: -38",
                "0002: -13",
                "0002: -13",
                "0002: -2"
            ]
        );
    }

    #[test]
    fn test_pointers() {
        assert_eq!(
            process_input("pint32 p\np[0]=0\np[1]=1"),
            vec!["2402: &0(0@,1i) 0 4 0", "2402: &0(0@,1i) 4 4 1"]
        );
        assert_eq!(
            process_input("pint32 p\nwait(p[0])"),
            vec!["2401: &0(0@,1i) 0 4 1@", "0001: 1@"]
        );

        assert_eq!(
            process_input("pint32 h, m\nh[0],m[0]=get_time_of_day()"),
            vec![
                "00BF: 1@ 2@",
                "2402: &0(0@,1i) 0 4 1@",
                "2402: &4(0@,1i) 0 4 2@",
            ]
        );

        assert_eq!(
            process_input(
                r#"
            pint arr
            int i,j
            arr[i] = arr[j]
            
            "#
            ),
            vec![
                "0A90: &8(0@,1i) 4 2@",
                "2401: &0(0@,1i) 2@ 4 1@",
                "0A90: &4(0@,1i) 4 3@",
                "0006: 2@ 1@",
                "2402: &0(0@,1i) 3@ 4 2@",
            ]
        );
        assert_eq!(
            process_input(
                r#"
            pint arr
            int i,j
            arr[i] = j+2
            
            "#
            ),
            vec![
                "0A90: &4(0@,1i) 4 2@",
                "0A8E: &8(0@,1i) 2 1@",
                "2402: &0(0@,1i) 2@ 4 1@",
            ]
        );
        assert_eq!(
            process_input("int a,b\npint x\nx[a],x[b]=x[b],x[a]"),
            vec![
                "0A90: &4(0@,1i) 4 2@",    //2@=b*4
                "2401: &8(0@,1i) 2@ 4 1@", //1@=x[b]
                "0A90: &0(0@,1i) 4 3@",    // 3@=a*4
                "0006: 2@ 1@",             //2@=x[b]
                "0A90: &0(0@,1i) 4 5@",    // 5@=a*4
                "2401: &8(0@,1i) 5@ 4 4@", // 4@=x[a]
                "0A90: &4(0@,1i) 4 6@",    // 6@=b*4
                "0006: 5@ 4@",             // 5@=x[a]
                "2402: &8(0@,1i) 3@ 4 2@", // x[a]=x[b]
                "2402: &8(0@,1i) 6@ 4 5@", // x[b]=x[a]
            ]
        )
    }

    #[test]
    fn test_const() {
        assert_eq!(process_input("const x = 10\nwait(x)"), vec!["0001: 10"]);

        assert_eq!(
            process_input("const x = 10\nconst y = 20\nwait(x+y)"),
            vec!["0001: 30"]
        );

        assert_eq!(
            process_input("const x = 10\nconst y = 20\nconst z = x+y\nwait(z)"),
            vec!["0001: 30"]
        );

        assert_eq!(
            process_input("const x,y,z = 1,2,3\nwait(x+y+z)"),
            vec!["0001: 6"]
        );

        assert_eq!(
            process_input("const x,y,z = 1,2,3\nint a,b,c=x,y,z\nwait(a+b+c)"),
            vec![
                "0006: &0(0@,1i) 1",
                "0006: &4(0@,1i) 2",
                "0006: &8(0@,1i) 3",
                "0A8E: &0(0@,1i) &4(0@,1i) 1@",
                "0A8E: 1@ &8(0@,1i) 2@",
                "0001: 2@",
            ]
        );

        assert_eq!(
            process_input("function foo\nconst x=1\nend\nfunction foo2\nconst x=1\nend"),
            vec!["004E: ", "2003: ", "2003: "]
        );

        assert_eq!(
            process_input("function foo\nconst a=1\nend\nconst a=1"),
            vec!["004E: ", "2003: "]
        );

        assert_eq!(process_input("int x = true"), vec!["0006: &0(0@,1i) 1"]);

        assert_eq!(process_input("int x = false"), vec!["0006: &0(0@,1i) 0"]);

        assert_eq!(
            process_input("function foo:int\nreturn true\nend"),
            vec!["004E: ", "2002: 1 1"]
        );

        assert_eq!(
            process_input("function foo:int\nreturn false\nend"),
            vec!["004E: ", "2002: 1 0"]
        );
    }

    #[test]
    fn test_timers() {
        assert_eq!(process_input("timera = 0"), vec!["0006: 32@ 0"]);

        assert_eq!(
            process_input("function foo\ntimera = 0\nend"),
            vec!["004E: ", "0006: 32@ 0", "2003: "]
        );
        assert_eq!(process_input("timerb = 0"), vec!["0006: 33@ 0"]);
    }
}
