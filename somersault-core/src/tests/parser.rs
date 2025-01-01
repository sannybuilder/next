#[cfg(test)]
mod tests {
    use crate::parser::*;
    use crate::tokens::*;
    use pretty_assertions::assert_eq;

    fn test_ast(tokens: Vec<TokenSpan>, stmts: Vec<AstNodeSpan>) {
        let ast = parse(tokens);
        assert!(ast.is_ok());

        assert_eq!(
            ast.unwrap(),
            AstNodeSpan {
                node: AstNode::Root(stmts),
                line: 1,
                start: 1,
            }
        )
    }

    #[test]
    fn basic_test() {
        let tokens = vec![
            TokenSpan {
                token: Token::Int(1),
                line: 1,
                start: 1,
                end: 2,
            },
            TokenSpan {
                token: Token::Eof,
                line: 1,
                start: 2,
                end: 2,
            },
        ];

        test_ast(
            tokens,
            vec![
                node_span_with(AstNode::NodeIntLiteral(1), 1, 1),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 2),
            ],
        )
    }

    #[test]
    fn basic_test2() {
        let tokens = vec![
            TokenSpan {
                token: Token::Int(1),
                line: 1,
                start: 1,
                end: 2,
            },
            TokenSpan {
                token: Token::Add,
                line: 1,
                start: 2,
                end: 3,
            },
            TokenSpan {
                token: Token::Int(2),
                line: 1,
                start: 3,
                end: 4,
            },
            TokenSpan {
                token: Token::Eof,
                line: 1,
                start: 4,
                end: 4,
            },
        ];

        test_ast(
            tokens,
            vec![
                node_span_with(AstNode::NodeIntLiteral(3), 1, 1),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 4),
            ],
        )
    }

    #[test]
    fn basic_test3() {
        let tokens = vec![
            TokenSpan {
                token: Token::Float(1.5),
                line: 1,
                start: 1,
                end: 4,
            },
            TokenSpan {
                token: Token::Sub,
                line: 1,
                start: 4,
                end: 5,
            },
            TokenSpan {
                token: Token::Float(2.0),
                line: 1,
                start: 5,
                end: 8,
            },
            TokenSpan {
                token: Token::Eof,
                line: 1,
                start: 8,
                end: 8,
            },
        ];

        test_ast(
            tokens,
            vec![
                node_span_with(AstNode::NodeFloatLiteral(-0.5), 1, 1),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 8),
            ],
        )
    }

    #[test]
    fn basic_test4() {
        let tokens = tokenize("a[4+3]".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                        name: node_span_with(AstNode::NodeIdentifier(String::from("a")), 1, 1),
                        index: node_span_with(AstNode::NodeIntLiteral(7), 1, 3),
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 7),
            ],
        )
    }

    #[test]
    fn basic_test5() {
        let tokens = tokenize("foo(1,2,3)".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeCallExpression(Box::new(NodeCallExpression {
                        name: node_span_with(AstNode::NodeIdentifier(String::from("foo")), 1, 1),
                        args: Some(node_span_with(
                            AstNode::NodeList(vec![
                                node_span_with(AstNode::NodeIntLiteral(1), 1, 5),
                                node_span_with(AstNode::NodeIntLiteral(2), 1, 7),
                                node_span_with(AstNode::NodeIntLiteral(3), 1, 9),
                            ]),
                            1,
                            5,
                        )),
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 11),
            ],
        );

        let tokens = tokenize("x = foo(1,2,3)".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeAssignmentStmt(Box::new(NodeAssignmentStmt {
                        lhs: node_span_with(AstNode::NodeIdentifier(String::from("x")), 1, 1),
                        rhs: node_span_with(
                            AstNode::NodeCallExpression(Box::new(NodeCallExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier(String::from("foo")),
                                    1,
                                    5,
                                ),
                                args: Some(node_span_with(
                                    AstNode::NodeList(vec![
                                        node_span_with(AstNode::NodeIntLiteral(1), 1, 9),
                                        node_span_with(AstNode::NodeIntLiteral(2), 1, 11),
                                        node_span_with(AstNode::NodeIntLiteral(3), 1, 13),
                                    ]),
                                    1,
                                    9,
                                )),
                            })),
                            1,
                            5,
                        ),
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 15),
            ],
        );

        let tokens = tokenize("x, y, z = foo(1,2,3)".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeAssignmentStmt(Box::new(NodeAssignmentStmt {
                        lhs: node_span_with(
                            AstNode::NodeList(vec![
                                node_span_with(AstNode::NodeIdentifier(String::from("x")), 1, 1),
                                node_span_with(AstNode::NodeIdentifier(String::from("y")), 1, 4),
                                node_span_with(AstNode::NodeIdentifier(String::from("z")), 1, 7),
                            ]),
                            1,
                            1,
                        ),
                        rhs: node_span_with(
                            AstNode::NodeCallExpression(Box::new(NodeCallExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier(String::from("foo")),
                                    1,
                                    11,
                                ),
                                args: Some(node_span_with(
                                    AstNode::NodeList(vec![
                                        node_span_with(AstNode::NodeIntLiteral(1), 1, 15),
                                        node_span_with(AstNode::NodeIntLiteral(2), 1, 17),
                                        node_span_with(AstNode::NodeIntLiteral(3), 1, 19),
                                    ]),
                                    1,
                                    15,
                                )),
                            })),
                            1,
                            11,
                        ),
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 21),
            ],
        );
    }

    #[test]
    fn basic_test7() {
        let tokens = tokenize("int x = -1".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIdentifier("x".to_string()),
                            line: 1,
                            start: 5,
                        }],
                        expr: Some(node_span_with(AstNode::NodeIntLiteral(-1), 1, 10)),
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 11),
            ],
        );

        let tokens = tokenize("int x = -x".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIdentifier("x".to_string()),
                            line: 1,
                            start: 5,
                        }],
                        expr: Some(node_span_with(
                            AstNode::NodeBinary(Box::new(NodeBinary {
                                lhs: node_span_with(AstNode::NodeIntLiteral(-1), 1, 10),
                                rhs: node_span_with(
                                    AstNode::NodeIdentifier("x".to_string()),
                                    1,
                                    10,
                                ),
                                op: Token::Mul,
                            })),
                            1,
                            10,
                        )),
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 11),
            ],
        );

        let tokens = tokenize("int x = return 5".as_bytes());

        assert!(tokens.is_ok());
        let ast = parse(tokens.unwrap());
        assert!(ast.is_err());
    }

    #[test]
    fn basic_test8() {
        let tokens = tokenize("int x = @label".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIdentifier("x".to_string()),
                            line: 1,
                            start: 5,
                        }],
                        expr: Some(node_span_with(
                            AstNode::NodeLabelLiteral("label".to_string()),
                            1,
                            9,
                        )),
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 15),
            ],
        );

        let tokens = tokenize("int x = @label + 1".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIdentifier("x".to_string()),
                            line: 1,
                            start: 5,
                        }],
                        expr: Some(node_span_with(
                            AstNode::NodeBinary(Box::new(NodeBinary {
                                lhs: node_span_with(
                                    AstNode::NodeLabelLiteral("label".to_string()),
                                    1,
                                    9,
                                ),
                                rhs: node_span_with(AstNode::NodeIntLiteral(1), 1, 18),
                                op: Token::Add,
                            })),
                            1,
                            9,
                        )),
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 19),
            ],
        );
    }

    #[test]
    fn array_test() {
        let tokens = tokenize("int x[3]".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier("x".to_string()),
                                    1,
                                    5,
                                ),
                                index: node_span_with(AstNode::NodeIntLiteral(3), 1, 7),
                            })),
                            line: 1,
                            start: 5,
                        }],
                        expr: None,
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 9),
            ],
        );

        let tokens = tokenize("int x[~-4]".as_bytes()); // not -4 == 3

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier("x".to_string()),
                                    1,
                                    5,
                                ),
                                index: node_span_with(
                                    AstNode::NodeUnary(Box::new(NodeUnary {
                                        node: node_span_with(AstNode::NodeIntLiteral(-4), 1, 9),
                                        op: Token::BitNot,
                                    })),
                                    1,
                                    7,
                                ),
                            })),
                            line: 1,
                            start: 5,
                        }],
                        expr: None,
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 11),
            ],
        );

        let tokens = tokenize("int x[3 & 1]".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier("x".to_string()),
                                    1,
                                    5,
                                ),
                                index: node_span_with(
                                    AstNode::NodeBinary(Box::new(NodeBinary {
                                        lhs: node_span_with(AstNode::NodeIntLiteral(3), 1, 7),
                                        rhs: node_span_with(AstNode::NodeIntLiteral(1), 1, 11),
                                        op: Token::BitAnd,
                                    })),
                                    1,
                                    7,
                                ),
                            })),
                            line: 1,
                            start: 5,
                        }],
                        expr: None,
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 13),
            ],
        );

        let tokens = tokenize("int x[3 | 1]".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier("x".to_string()),
                                    1,
                                    5,
                                ),
                                index: node_span_with(
                                    AstNode::NodeBinary(Box::new(NodeBinary {
                                        lhs: node_span_with(AstNode::NodeIntLiteral(3), 1, 7),
                                        rhs: node_span_with(AstNode::NodeIntLiteral(1), 1, 11),
                                        op: Token::BitOr,
                                    })),
                                    1,
                                    7,
                                ),
                            })),
                            line: 1,
                            start: 5,
                        }],
                        expr: None,
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 13),
            ],
        );

        let tokens = tokenize("int x[3 ^ 0]".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier("x".to_string()),
                                    1,
                                    5,
                                ),
                                index: node_span_with(
                                    AstNode::NodeBinary(Box::new(NodeBinary {
                                        lhs: node_span_with(AstNode::NodeIntLiteral(3), 1, 7),
                                        rhs: node_span_with(AstNode::NodeIntLiteral(0), 1, 11),
                                        op: Token::BitXor,
                                    })),
                                    1,
                                    7,
                                ),
                            })),
                            line: 1,
                            start: 5,
                        }],
                        expr: None,
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 13),
            ],
        );

        let tokens = tokenize("int x[2 + 2 * 4 / 2 - 1]".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier("x".to_string()),
                                    1,
                                    5,
                                ),
                                index: node_span_with(AstNode::NodeIntLiteral(5), 1, 6),
                            })),
                            line: 1,
                            start: 5,
                        }],
                        expr: None,
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 25),
            ],
        );
    }

    #[test]
    fn multi_init() {
        let tokens = tokenize("int a, b = 1, 2".as_bytes());
        assert!(tokens.is_ok());
        let ast = parse(tokens.unwrap());
        /*
               this example fails due to ambiguity
               the comma can be viewed as both the value list separator (b, c)
               and the declaration statement separator (int a, int b, int c)
               producing two different ASTs:
               1: int a; int b; b = 1; 2;
               2: int a; int b; a = 1; b = 2;

               currently parser chooses the first variant resulting in a orphan `2`

               update 10/18/24: ambiguity is resolved in favor of the second variant
        */
        // assert!(ast.is_err());
        assert!(ast.is_ok());
    }
    #[test]
    fn string_test() {
        let tokens = tokenize("string x = 'hello'".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIdentifier("x".to_string()),
                            line: 1,
                            start: 8,
                        }],
                        expr: Some(node_span_with(
                            AstNode::NodeStringLiteral(vec![104, 101, 108, 108, 111]),
                            1,
                            12,
                        )),
                        ty: ArgType::String,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 19),
            ],
        );

        let tokens = tokenize("string x = \"hello\"".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIdentifier("x".to_string()),
                            line: 1,
                            start: 8,
                        }],
                        expr: Some(node_span_with(
                            AstNode::NodeStringLiteral(vec![104, 101, 108, 108, 111]),
                            1,
                            12,
                        )),
                        ty: ArgType::String,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 19),
            ],
        );
    }

    #[test]
    fn bit_test() {
        let tokens = tokenize("a + 2 & b".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeBinary(Box::new(NodeBinary {
                        lhs: node_span_with(
                            AstNode::NodeBinary(Box::new(NodeBinary {
                                lhs: node_span_with(AstNode::NodeIdentifier("a".to_string()), 1, 1),
                                rhs: node_span_with(AstNode::NodeIntLiteral(2), 1, 5),
                                op: Token::Add,
                            })),
                            1,
                            1,
                        ),
                        rhs: node_span_with(AstNode::NodeIdentifier("b".to_string()), 1, 9),
                        op: Token::BitAnd,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 10),
            ],
        );

        let tokens = tokenize("~b".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeUnary(Box::new(NodeUnary {
                        node: node_span_with(AstNode::NodeIdentifier("b".to_string()), 1, 2),
                        op: Token::BitNot,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 3),
            ],
        );

        let tokens = tokenize("~b[0]".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeUnary(Box::new(NodeUnary {
                        node: node_span_with(
                            AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier("b".to_string()),
                                    1,
                                    2,
                                ),
                                index: node_span_with(AstNode::NodeIntLiteral(0), 1, 4),
                            })),
                            1,
                            2,
                        ),
                        op: Token::BitNot,
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 6),
            ],
        );
    }

    #[test]
    fn test_unary() {
        let tokens = tokenize("-5".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(AstNode::NodeIntLiteral(-5), 1, 2),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 3),
            ],
        );

        let tokens = tokenize("-a".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeBinary(Box::new(NodeBinary {
                        lhs: node_span_with(AstNode::NodeIntLiteral(-1), 1, 2),
                        rhs: node_span_with(AstNode::NodeIdentifier(String::from("a")), 1, 2),
                        op: Token::Mul,
                    })),
                    1,
                    2,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 3),
            ],
        );

        let tokens = tokenize("-a[0]".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeBinary(Box::new(NodeBinary {
                        lhs: node_span_with(AstNode::NodeIntLiteral(-1), 1, 2),
                        rhs: node_span_with(
                            AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier(String::from("a")),
                                    1,
                                    2,
                                ),
                                index: node_span_with(AstNode::NodeIntLiteral(0), 1, 4),
                            })),
                            1,
                            2,
                        ),
                        op: Token::Mul,
                    })),
                    1,
                    2,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 6),
            ],
        );

        let tokens = tokenize("-(5+5)".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(AstNode::NodeIntLiteral(-10), 1, 3),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 7),
            ],
        );

        let tokens = tokenize("-5+5".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(AstNode::NodeIntLiteral(0), 1, 2),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 5),
            ],
        );

        let tokens = tokenize("-foo()".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeBinary(Box::new(NodeBinary {
                        lhs: node_span_with(AstNode::NodeIntLiteral(-1), 1, 2),
                        rhs: node_span_with(
                            AstNode::NodeCallExpression(Box::new(NodeCallExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier("foo".to_string()),
                                    1,
                                    2,
                                ),
                                args: None,
                            })),
                            1,
                            2,
                        ),
                        op: Token::Mul,
                    })),
                    1,
                    2,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 7),
            ],
        );
    }

    #[test]
    fn test_functions() {
        let tokens = tokenize("function x\nend".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeFuncDeclStml(Box::new(NodeFuncDeclStml {
                        name: "x".to_string(),
                        args: vec![],
                        return_types: vec![],
                        is_optional: false,
                        is_logical: false,
                        is_exported: false,
                        body: vec![node_span_with(AstNode::NodeDefaultReturnStmt, 2, 1)],
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 2, 4),
            ],
        );

        let tokens = tokenize("function x:logical\nend".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeFuncDeclStml(Box::new(NodeFuncDeclStml {
                        name: "x".to_string(),
                        args: vec![],
                        return_types: vec![],
                        is_optional: false,
                        is_logical: true,
                        is_exported: false,
                        body: vec![node_span_with(AstNode::NodeDefaultReturnStmt, 2, 1)],
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 2, 4),
            ],
        );

        let tokens = tokenize("function x:optional int\nend".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeFuncDeclStml(Box::new(NodeFuncDeclStml {
                        name: "x".to_string(),
                        args: vec![],
                        return_types: vec!["int".to_string()],
                        is_optional: true,
                        is_logical: false,
                        is_exported: false,
                        body: vec![node_span_with(AstNode::NodeDefaultReturnStmt, 2, 1)],
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 2, 4),
            ],
        );
    }

    #[test]
    fn test_comment() {
        let tokens = tokenize("wait(/*1*/0)".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeCallExpression(Box::new(NodeCallExpression {
                        name: node_span_with(AstNode::NodeIdentifier(String::from("wait")), 1, 1),
                        args: Some(node_span_with(AstNode::NodeIntLiteral(0), 1, 11)),
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 13),
            ],
        );
    }

    #[test]
    fn test_return() {
        let tokens = tokenize("return 1".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeReturnStmt(Box::new(NodeReturnStmt {
                        expr: Some(node_span_with(AstNode::NodeIntLiteral(1), 1, 8)),
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 9),
            ],
        );

        let tokens = tokenize("return".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeReturnStmt(Box::new(NodeReturnStmt { expr: None })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 7),
            ],
        );

        let tokens = tokenize("return 1,2".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeReturnStmt(Box::new(NodeReturnStmt {
                        expr: Some(node_span_with(
                            AstNode::NodeList(vec![
                                node_span_with(AstNode::NodeIntLiteral(1), 1, 8),
                                node_span_with(AstNode::NodeIntLiteral(2), 1, 10),
                            ]),
                            1,
                            8,
                        )),
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 11),
            ],
        );

        let tokens =
            tokenize("return get_player_coordinates(0), get_player_coordinates(1)".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeReturnStmt(Box::new(NodeReturnStmt {
                        expr: Some(node_span_with(
                            AstNode::NodeList(vec![
                                node_span_with(
                                    AstNode::NodeCallExpression(Box::new(NodeCallExpression {
                                        name: node_span_with(
                                            AstNode::NodeIdentifier(
                                                "get_player_coordinates".to_string(),
                                            ),
                                            1,
                                            8,
                                        ),
                                        args: Some(node_span_with(
                                            AstNode::NodeIntLiteral(0),
                                            1,
                                            31,
                                        )),
                                    })),
                                    1,
                                    8,
                                ),
                                node_span_with(
                                    AstNode::NodeCallExpression(Box::new(NodeCallExpression {
                                        name: node_span_with(
                                            AstNode::NodeIdentifier(
                                                "get_player_coordinates".to_string(),
                                            ),
                                            1,
                                            35,
                                        ),
                                        args: Some(node_span_with(
                                            AstNode::NodeIntLiteral(1),
                                            1,
                                            58,
                                        )),
                                    })),
                                    1,
                                    35,
                                ),
                            ]),
                            1,
                            8,
                        )),
                    })),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 60),
            ],
        );
    }

    #[test]
    fn test_list() {
        let tokens = tokenize("1, 2, 3".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeList(vec![
                        node_span_with(AstNode::NodeIntLiteral(1), 1, 1),
                        node_span_with(AstNode::NodeIntLiteral(2), 1, 4),
                        node_span_with(AstNode::NodeIntLiteral(3), 1, 7),
                    ]),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 8),
            ],
        );

        let tokens = tokenize("1+1, 2+2, 3+3".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeList(vec![
                        node_span_with(AstNode::NodeIntLiteral(2), 1, 1),
                        node_span_with(AstNode::NodeIntLiteral(4), 1, 6),
                        node_span_with(AstNode::NodeIntLiteral(6), 1, 11),
                    ]),
                    1,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 1, 14),
            ],
        );

        let tokens = tokenize("int a[3]\na[1],a[2]+a[3]".as_bytes());

        test_ast(
            tokens.unwrap(),
            vec![
                node_span_with(
                    AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                        names: vec![AstNodeSpan {
                            node: AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier("a".to_string()),
                                    1,
                                    5,
                                ),
                                index: node_span_with(AstNode::NodeIntLiteral(3), 1, 6),
                            })),
                            line: 1,
                            start: 5,
                        }],
                        expr: None,
                        ty: ArgType::Int,
                    })),
                    1,
                    1,
                ),
                node_span_with(
                    AstNode::NodeList(vec![
                        node_span_with(
                            AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                name: node_span_with(
                                    AstNode::NodeIdentifier("a".to_string()),
                                    2,
                                    1,
                                ),
                                index: node_span_with(AstNode::NodeIntLiteral(1), 2, 3),
                            })),
                            2,
                            1,
                        ),
                        node_span_with(
                            AstNode::NodeBinary(Box::new(NodeBinary {
                                lhs: node_span_with(
                                    AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                        name: node_span_with(
                                            AstNode::NodeIdentifier("a".to_string()),
                                            2,
                                            6,
                                        ),
                                        index: node_span_with(AstNode::NodeIntLiteral(2), 2, 8),
                                    })),
                                    2,
                                    6,
                                ),
                                rhs: node_span_with(
                                    AstNode::NodeIndexExpression(Box::new(NodeIndexExpression {
                                        name: node_span_with(
                                            AstNode::NodeIdentifier("a".to_string()),
                                            2,
                                            11,
                                        ),
                                        index: node_span_with(AstNode::NodeIntLiteral(3), 2, 13),
                                    })),
                                    2,
                                    11,
                                ),
                                op: Token::Add,
                            })),
                            2,
                            6,
                        ),
                    ]),
                    2,
                    1,
                ),
                node_span_with(AstNode::NodeDefaultReturnStmt, 2, 15),
            ],
        );
    }

    #[test]
    fn parse_if_statement() {
        let tokens = tokenize("if 1 then\n 2\n\n\nelse 3 \n\n\nend".as_bytes());

        assert!(tokens.is_ok());
        let ast = parse(tokens.unwrap());
        assert!(ast.is_ok());

        let tokens = tokenize("if 1 then\n 2 \n\n\nend".as_bytes());

        assert!(tokens.is_ok());
        let ast = parse(tokens.unwrap());
        assert!(ast.is_ok());

        let tokens = tokenize("if 1\nthen\n 2 \n\n\nend".as_bytes());

        assert!(tokens.is_ok());
        let ast = parse(tokens.unwrap());
        assert!(ast.is_ok());

        let tokens = tokenize("if 1\nthen 2\nelse\nend".as_bytes());

        assert!(tokens.is_ok());
        let ast = parse(tokens.unwrap());
        assert!(ast.is_ok());

        let tokens = tokenize("is_player_playing(0) or is_player_playing(0)".as_bytes());

        assert!(tokens.is_ok());
        let ast = parse(tokens.unwrap());
        assert!(ast.is_ok());

        let tokens =
            tokenize("if is_player_playing(0) or is_player_playing(0) then\nend".as_bytes());

        assert!(tokens.is_ok());
        let ast = parse(tokens.unwrap());
        assert!(ast.is_ok());
    }

    #[test]
    fn compound_assignment() {
        macro_rules! test {
            ($c1: expr, $c2: expr) => {{
                let tokens = tokenize(format!("int x\nx {} 1", $c1).as_bytes());

                test_ast(
                    tokens.unwrap(),
                    vec![
                        node_span_with(
                            AstNode::NodeVarDeclStml(Box::new(NodeVarDeclStml {
                                names: vec![AstNodeSpan {
                                    node: AstNode::NodeIdentifier("x".to_string()),
                                    line: 1,
                                    start: 5,
                                }],
                                expr: None,
                                ty: ArgType::Int,
                            })),
                            1,
                            1,
                        ),
                        node_span_with(
                            AstNode::NodeAssignmentStmt(Box::new(NodeAssignmentStmt {
                                lhs: node_span_with(
                                    AstNode::NodeIdentifier(String::from("x")),
                                    2,
                                    1,
                                ),
                                rhs: node_span_with(
                                    AstNode::NodeBinary(Box::new(NodeBinary {
                                        lhs: node_span_with(
                                            AstNode::NodeIdentifier("x".to_string()),
                                            2,
                                            1,
                                        ),
                                        rhs: node_span_with(AstNode::NodeIntLiteral(1), 2, 6),
                                        op: $c2,
                                    })),
                                    2,
                                    1,
                                ),
                            })),
                            2,
                            1,
                        ),
                        node_span_with(AstNode::NodeDefaultReturnStmt, 2, 7),
                    ],
                )
            }};
        }

        test!(Token::AddEqual, Token::Add);
        test!(Token::SubEqual, Token::Sub);
        test!(Token::MulEqual, Token::Mul);
        test!(Token::DivEqual, Token::Div);
        test!(Token::BitAndEqual, Token::BitAnd);
        test!(Token::BitOrEqual, Token::BitOr);
        test!(Token::BitXorEqual, Token::BitXor);
    }
}
