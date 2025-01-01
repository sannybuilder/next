#[cfg(test)]
mod tests {
    use crate::tokens::*;
    use pretty_assertions::assert_eq;
    #[test]
    fn basic_test() {
        assert_eq!(
            tokenize("".as_bytes()).unwrap(),
            vec![TokenSpan {
                token: Token::Eof,
                line: 1,
                start: 1,
                end: 1
            }]
        );
        assert_eq!(
            tokenize("  ".as_bytes()).unwrap(),
            vec![TokenSpan {
                token: Token::Eof,
                line: 1,
                start: 3,
                end: 3
            }]
        );
        assert_eq!(
            tokenize("\n \n ".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::NewLine,
                    line: 1,
                    start: 1,
                    end: 2
                },
                TokenSpan {
                    token: Token::NewLine,
                    line: 2,
                    start: 2,
                    end: 3
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 3,
                    start: 2,
                    end: 2
                }
            ]
        );
        assert_eq!(
            tokenize(" 12345 123.45 -5".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::Int(12345),
                    line: 1,
                    start: 2,
                    end: 7
                },
                TokenSpan {
                    token: Token::Float(123.45),
                    line: 1,
                    start: 8,
                    end: 14,
                },
                TokenSpan {
                    token: Token::Sub,
                    line: 1,
                    start: 15,
                    end: 16
                },
                TokenSpan {
                    token: Token::Int(5),
                    line: 1,
                    start: 16,
                    end: 17
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 17,
                    end: 17
                }
            ]
        );

        assert_eq!(
            tokenize(" 1 + 2 - 3 * 4 / 5 , ( ) [ ] = \n".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::Int(1),
                    line: 1,
                    start: 2,
                    end: 3
                },
                TokenSpan {
                    token: Token::Add,
                    line: 1,
                    start: 4,
                    end: 5
                },
                TokenSpan {
                    token: Token::Int(2),
                    line: 1,
                    start: 6,
                    end: 7
                },
                TokenSpan {
                    token: Token::Sub,
                    line: 1,
                    start: 8,
                    end: 9
                },
                TokenSpan {
                    token: Token::Int(3),
                    line: 1,
                    start: 10,
                    end: 11
                },
                TokenSpan {
                    token: Token::Mul,
                    line: 1,
                    start: 12,
                    end: 13
                },
                TokenSpan {
                    token: Token::Int(4),
                    line: 1,
                    start: 14,
                    end: 15
                },
                TokenSpan {
                    token: Token::Div,
                    line: 1,
                    start: 16,
                    end: 17
                },
                TokenSpan {
                    token: Token::Int(5),
                    line: 1,
                    start: 18,
                    end: 19
                },
                TokenSpan {
                    token: Token::Comma,
                    line: 1,
                    start: 20,
                    end: 21
                },
                TokenSpan {
                    token: Token::OpenParen,
                    line: 1,
                    start: 22,
                    end: 23
                },
                TokenSpan {
                    token: Token::CloseParen,
                    line: 1,
                    start: 24,
                    end: 25
                },
                TokenSpan {
                    token: Token::OpenBracket,
                    line: 1,
                    start: 26,
                    end: 27
                },
                TokenSpan {
                    token: Token::CloseBracket,
                    line: 1,
                    start: 28,
                    end: 29
                },
                TokenSpan {
                    token: Token::Equal,
                    line: 1,
                    start: 30,
                    end: 31
                },
                TokenSpan {
                    token: Token::NewLine,
                    line: 1,
                    start: 32,
                    end: 33
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 2,
                    start: 1,
                    end: 1
                },
            ]
        );

        assert_eq!(
            tokenize("a a5 5 int float string".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::Ident(String::from("a")),
                    line: 1,
                    start: 1,
                    end: 2
                },
                TokenSpan {
                    token: Token::Ident(String::from("a5")),
                    line: 1,
                    start: 3,
                    end: 5
                },
                TokenSpan {
                    token: Token::Int(5),
                    line: 1,
                    start: 6,
                    end: 7
                },
                TokenSpan {
                    token: Token::Kwd(Keyword::Int),
                    line: 1,
                    start: 8,
                    end: 11
                },
                TokenSpan {
                    token: Token::Kwd(Keyword::Float),
                    line: 1,
                    start: 12,
                    end: 17
                },
                TokenSpan {
                    token: Token::Kwd(Keyword::String),
                    line: 1,
                    start: 18,
                    end: 24
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 24,
                    end: 24
                },
            ]
        );

        assert_eq!(
            tokenize("'abc' 'a\\'bc' 'bca'".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::String(vec![b'a', b'b', b'c']),
                    line: 1,
                    start: 1,
                    end: 6
                },
                TokenSpan {
                    token: Token::String(vec![b'a', b'\\', b'\'', b'b', b'c']),
                    line: 1,
                    start: 7,
                    end: 14
                },
                TokenSpan {
                    token: Token::String(vec![b'b', b'c', b'a']),
                    line: 1,
                    start: 15,
                    end: 20
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 20,
                    end: 20
                },
            ]
        );

        assert_eq!(
            tokenize(r#""abc" "a\"bc""#.as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::String(vec![b'a', b'b', b'c']),
                    line: 1,
                    start: 1,
                    end: 6
                },
                TokenSpan {
                    token: Token::String(vec![b'a', b'\\', b'"', b'b', b'c']),
                    line: 1,
                    start: 7,
                    end: 14
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 14,
                    end: 14
                },
            ]
        );

        assert_eq!(
            tokenize("0xABC 0b1010 -0x01 -0b111".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::Int(0xABC),
                    line: 1,
                    start: 1,
                    end: 6
                },
                TokenSpan {
                    token: Token::Int(0b1010),
                    line: 1,
                    start: 7,
                    end: 13
                },
                TokenSpan {
                    token: Token::Sub,
                    line: 1,
                    start: 14,
                    end: 15
                },
                TokenSpan {
                    token: Token::Int(0x01),
                    line: 1,
                    start: 15,
                    end: 19
                },
                TokenSpan {
                    token: Token::Sub,
                    line: 1,
                    start: 20,
                    end: 21
                },
                TokenSpan {
                    token: Token::Int(0b111),
                    line: 1,
                    start: 21,
                    end: 26
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 26,
                    end: 26
                },
            ]
        );

        assert_eq!(
            tokenize("& | ^ ~".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::BitAnd,
                    line: 1,
                    start: 1,
                    end: 2
                },
                TokenSpan {
                    token: Token::BitOr,
                    line: 1,
                    start: 3,
                    end: 4
                },
                TokenSpan {
                    token: Token::BitXor,
                    line: 1,
                    start: 5,
                    end: 6
                },
                TokenSpan {
                    token: Token::BitNot,
                    line: 1,
                    start: 7,
                    end: 8
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 8,
                    end: 8
                },
            ]
        );

        assert_eq!(
            tokenize("@label".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::Label(String::from("label")),
                    line: 1,
                    start: 1,
                    end: 7
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 7,
                    end: 7
                }
            ]
        );
    }

    #[test]
    fn basic_comment() {
        assert_eq!(
            tokenize("1 // 2".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::Int(1),
                    line: 1,
                    start: 1,
                    end: 2
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 7,
                    end: 7
                }
            ]
        );

        assert_eq!(
            tokenize("1 /* 2 */".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::Int(1),
                    line: 1,
                    start: 1,
                    end: 2
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 10,
                    end: 10
                }
            ]
        );

        assert_eq!(
            tokenize("1 /* 2 ".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::Int(1),
                    line: 1,
                    start: 1,
                    end: 2
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 8,
                    end: 8
                }
            ]
        );

        assert_eq!(
            tokenize("wait(/*1*/0)".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::Ident(String::from("wait")),
                    line: 1,
                    start: 1,
                    end: 5
                },
                TokenSpan {
                    token: Token::OpenParen,
                    line: 1,
                    start: 5,
                    end: 6
                },
                TokenSpan {
                    token: Token::Int(0),
                    line: 1,
                    start: 11,
                    end: 12
                },
                TokenSpan {
                    token: Token::CloseParen,
                    line: 1,
                    start: 12,
                    end: 13
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 13,
                    end: 13
                }
            ]
        );

        assert_eq!(
            tokenize("and or not".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::And,
                    line: 1,
                    start: 1,
                    end: 4
                },
                TokenSpan {
                    token: Token::Or,
                    line: 1,
                    start: 5,
                    end: 7
                },
                TokenSpan {
                    token: Token::Not,
                    line: 1,
                    start: 8,
                    end: 11
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 11,
                    end: 11
                }
            ]
        );

        assert_eq!(
            tokenize("+= -= *= /= &= |= ^=".as_bytes()).unwrap(),
            vec![
                TokenSpan {
                    token: Token::AddEqual,
                    line: 1,
                    start: 1,
                    end: 3
                },
                TokenSpan {
                    token: Token::SubEqual,
                    line: 1,
                    start: 4,
                    end: 6
                },
                TokenSpan {
                    token: Token::MulEqual,
                    line: 1,
                    start: 7,
                    end: 9
                },
                TokenSpan {
                    token: Token::DivEqual,
                    line: 1,
                    start: 10,
                    end: 12
                },
                TokenSpan {
                    token: Token::BitAndEqual,
                    line: 1,
                    start: 13,
                    end: 15
                },
                TokenSpan {
                    token: Token::BitOrEqual,
                    line: 1,
                    start: 16,
                    end: 18
                },
                TokenSpan {
                    token: Token::BitXorEqual,
                    line: 1,
                    start: 19,
                    end: 21
                },
                TokenSpan {
                    token: Token::Eof,
                    line: 1,
                    start: 21,
                    end: 21
                }
            ]
        );
    }
}
