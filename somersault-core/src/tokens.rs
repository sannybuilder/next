use std::fmt::{Display, Formatter};

use anyhow::{bail, Result};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Eof,
    Int(i32),
    Float(f32),
    Label(String),
    String(Vec<u8>),
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    And,
    Or,
    Not,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Comma,
    Colon,
    Space,
    NewLine,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,
    EqualEqual,
    NotEqual,
    AddEqual,
    SubEqual,
    MulEqual,
    DivEqual,
    BitAndEqual,
    BitOrEqual,
    BitXorEqual,
    Ident(String),
    Kwd(Keyword),
    Unknown(char),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenSpan {
    pub token: Token,
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Keyword {
    Int,
    PInt32,
    Float,
    String,
    Return,
    Function,
    Optional,
    Logical,
    End,
    If,
    Then,
    Else,
    While,
    Break,
    Continue,
    Const,
    Export,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Keyword::Int => write!(f, "int"),
            Keyword::PInt32 => write!(f, "pInt32"),
            Keyword::Float => write!(f, "float"),
            Keyword::String => write!(f, "string"),
            Keyword::Return => write!(f, "return"),
            Keyword::Function => write!(f, "function"),
            Keyword::Optional => write!(f, "optional"),
            Keyword::Logical => write!(f, "logical"),
            Keyword::End => write!(f, "end"),
            Keyword::If => write!(f, "if"),
            Keyword::Then => write!(f, "then"),
            Keyword::Else => write!(f, "else"),
            Keyword::While => write!(f, "while"),
            Keyword::Break => write!(f, "break"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Const => write!(f, "const"),
            Keyword::Export => write!(f, "export"),
        }
    }
}

fn keyword_lookup(name: String) -> Token {
    match name.to_ascii_lowercase().as_str() {
        "int" => Token::Kwd(Keyword::Int),
        "pint" => Token::Kwd(Keyword::PInt32),
        "pint32" => Token::Kwd(Keyword::PInt32),
        "float" => Token::Kwd(Keyword::Float),
        "string" => Token::Kwd(Keyword::String),
        "return" => Token::Kwd(Keyword::Return),
        "function" => Token::Kwd(Keyword::Function),
        "optional" => Token::Kwd(Keyword::Optional),
        "logical" => Token::Kwd(Keyword::Logical),
        "end" => Token::Kwd(Keyword::End),
        "if" => Token::Kwd(Keyword::If),
        "then" => Token::Kwd(Keyword::Then),
        "else" => Token::Kwd(Keyword::Else),
        "while" => Token::Kwd(Keyword::While),
        "break" => Token::Kwd(Keyword::Break),
        "continue" => Token::Kwd(Keyword::Continue),
        "const" => Token::Kwd(Keyword::Const),
        "export" => Token::Kwd(Keyword::Export),
        "and" => Token::And,
        "or" => Token::Or,
        "not" => Token::Not,
        _ => Token::Ident(name),
    }
}

pub fn tokenize(input: &[u8]) -> Result<Vec<TokenSpan>> {
    let mut tokens = vec![];
    let mut index = 0;
    let mut line = 1;
    let mut token_start = index;
    let mut line_start = 0;

    macro_rules! push {
        ($token: expr) => {
            let start = token_start - line_start + 1;
            let end = index - line_start + 1;
            tokens.push(TokenSpan {
                token: $token,
                line,
                start,
                end,
            });
        };
    }

    while let Some(token) = get_next_token(input, &mut index, &mut line) {
        match token {
            Token::Unknown(ch) => {
                bail!(
                    "Unknown token found: {ch} at line {} col {}",
                    line,
                    token_start - line_start + 1
                );
            }
            Token::NewLine => {
                push!(token);

                line += 1;
                line_start = index;
                token_start = index;
            }
            Token::Space => {
                token_start = index;
            }
            _ => {
                push!(token);
                token_start = index;
            }
        };
    }
    push!(Token::Eof);
    return Ok(tokens);
}

fn get_next_token(input: &[u8], index: &mut usize, line: &mut usize) -> Option<Token> {
    macro_rules! advance {
        () => {
            *index += 1
        };
        ($n: expr) => {
            *index += $n
        };
    }
    macro_rules! cur {
        () => {
            input.get(*index)
        };
    }
    macro_rules! next {
        () => {
            input.get(*index + 1)
        };
    }
    match cur!() {
        None => None,
        Some(x) => Some(match x {
            b'\t' | b' ' => {
                while let Some(b'\t' | b' ') = cur!() {
                    advance!()
                }
                Token::Space
            }
            b'\r' => {
                advance!();
                if cur!() == Some(&b'\n') {
                    advance!();
                }
                Token::NewLine
            }
            b'\n' => {
                advance!();
                Token::NewLine
            }
            b'0'..=b'9' => match (cur!(), next!()) {
                (Some(&b'0'), Some(&b'x')) => {
                    advance!(2);
                    let start = *index;
                    loop {
                        match cur!() {
                            Some(b'0'..=b'9') | Some(b'a'..=b'f') | Some(b'A'..=b'F') => {
                                advance!()
                            }
                            _ => break,
                        }
                    }
                    unsafe {
                        let s = std::str::from_utf8_unchecked(&input[start..*index]);
                        u32::from_str_radix(s, 16)
                            .ok()
                            .map(|i| Token::Int(std::mem::transmute(i)))?
                    }
                }
                (Some(&b'0'), Some(&b'b')) => {
                    advance!(2);
                    let start = *index;
                    loop {
                        match cur!() {
                            Some(b'0') | Some(b'1') => {
                                advance!()
                            }
                            _ => break,
                        }
                    }
                    unsafe {
                        let s = std::str::from_utf8_unchecked(&input[start..*index]);
                        u32::from_str_radix(s, 2)
                            .ok()
                            .map(|i| Token::Int(std::mem::transmute(i)))?
                    }
                }
                _ => {
                    let start = *index;
                    advance!();
                    let mut is_float = false;
                    loop {
                        match cur!() {
                            Some(b'0'..=b'9') => {
                                advance!()
                            }
                            Some(b'.') => {
                                advance!();
                                is_float = true;
                            }
                            _ => break,
                        }
                    }

                    unsafe {
                        let s = std::str::from_utf8_unchecked(&input[start..*index]);
                        if is_float {
                            s.parse::<f32>().ok().map(|f| Token::Float(f))?
                        } else {
                            s.parse::<i32>().ok().map(|i| Token::Int(i))?
                        }
                    }
                }
            },
            b'+' => {
                advance!();
                match cur!() {
                    Some(b'=') => {
                        advance!();
                        Token::AddEqual
                    }
                    _ => Token::Add,
                }
            }
            b'-' => {
                advance!();
                match cur!() {
                    Some(b'=') => {
                        advance!();
                        Token::SubEqual
                    }
                    _ => Token::Sub,
                }
            }
            b'*' => {
                advance!();
                match cur!() {
                    Some(b'=') => {
                        advance!();
                        Token::MulEqual
                    }
                    _ => Token::Mul,
                }
            }
            b'=' => {
                advance!();
                match cur!() {
                    Some(b'=') => {
                        advance!();
                        Token::EqualEqual
                    }
                    _ => Token::Equal,
                }
            }
            b'/' => {
                advance!();
                match cur!() {
                    Some(&b'/') => loop {
                        // line comments
                        advance!();
                        if let Some(b'\n') | None = cur!() {
                            return Some(Token::Space);
                        }
                    },
                    Some(&b'=') => {
                        advance!();
                        Token::DivEqual
                    }
                    Some(&b'*') => {
                        /* inline comments */
                        loop {
                            //
                            advance!();
                            match cur!() {
                                Some(b'\n') => {
                                    *line += 1;
                                }
                                Some(b'*') if next!() == Some(&b'/') => {
                                    advance!(2);
                                    return Some(Token::Space);
                                }
                                None => {
                                    return Some(Token::Space);
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => Token::Div,
                }
            }
            b'(' => {
                advance!();
                Token::OpenParen
            }
            b')' => {
                advance!();
                Token::CloseParen
            }
            b'[' => {
                advance!();
                Token::OpenBracket
            }
            b']' => {
                advance!();
                Token::CloseBracket
            }
            b',' => {
                advance!();
                Token::Comma
            }
            b'&' => {
                advance!();
                match cur!() {
                    Some(b'=') => {
                        advance!();
                        Token::BitAndEqual
                    }
                    _ => Token::BitAnd,
                }
            }
            b'|' => {
                advance!();
                match cur!() {
                    Some(b'=') => {
                        advance!();
                        Token::BitOrEqual
                    }
                    _ => Token::BitOr,
                }
            }
            b'^' => {
                advance!();
                match cur!() {
                    Some(b'=') => {
                        advance!();
                        Token::BitXorEqual
                    }
                    _ => Token::BitXor,
                }
            }
            b'~' => {
                advance!();
                Token::BitNot
            }
            b':' => {
                advance!();
                Token::Colon
            }
            b'@' => {
                advance!();
                let start = *index;
                while let Some(b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9') = cur!() {
                    advance!()
                }

                unsafe {
                    let s = String::from_utf8_unchecked(input[start..*index].to_vec());
                    Token::Label(s)
                }
            }
            b'\'' => {
                advance!();
                let start = *index;
                loop {
                    match cur!() {
                        Some(b'\\') => {
                            advance!(2);
                        }
                        Some(b'\'') => {
                            break;
                        }
                        Some(_) => advance!(),
                        None => break,
                    }
                }

                match cur!() {
                    Some(b'\'') => {
                        advance!();
                        Token::String(input[start..*index - 1].to_vec())
                    }
                    Some(x) => return Some(Token::Unknown((*x).into())),
                    None => Token::Unknown(0.into()),
                }
            }
            b'"' => {
                advance!();
                let start = *index;
                loop {
                    match cur!() {
                        Some(b'\\') => {
                            advance!(2);
                        }
                        Some(b'"') => {
                            break;
                        }
                        Some(_) => advance!(),
                        None => break,
                    }
                }

                match cur!() {
                    Some(b'"') => {
                        advance!();
                        Token::String(input[start..*index - 1].to_vec())
                    }
                    Some(x) => return Some(Token::Unknown((*x).into())),
                    None => Token::Unknown(0.into()),
                }
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let start = *index;
                advance!();
                while let Some(b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9') = cur!() {
                    advance!()
                }

                unsafe {
                    let s = String::from_utf8_unchecked(input[start..*index].to_vec());
                    keyword_lookup(s)
                }
            }
            b'>' => {
                advance!();
                match cur!() {
                    Some(b'=') => {
                        advance!();
                        Token::GreaterEqual
                    }
                    _ => Token::Greater,
                }
            }
            b'<' => {
                advance!();
                match cur!() {
                    Some(b'=') => {
                        advance!();
                        Token::LessEqual
                    }
                    Some(b'>') => {
                        advance!();
                        Token::NotEqual
                    }
                    _ => Token::Less,
                }
            }
            unknown => Token::Unknown((*unknown).into()),
        }),
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::Eof => write!(f, "end of file"),
            Token::Space => write!(f, " "),
            Token::Int(i) => write!(f, "{}", i),
            Token::Float(fl) => {
                //ensure that the float is displayed with .0 if it is a whole number
                if fl.fract() == 0.0 {
                    write!(f, "{:.1}", fl)
                } else {
                    write!(f, "{}", fl)
                }
            }
            Token::Label(s) => write!(f, "@{}", s),
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Equal => write!(f, "="),
            Token::BitAnd => write!(f, "&"),
            Token::BitOr => write!(f, "|"),
            Token::BitXor => write!(f, "^"),
            Token::BitNot => write!(f, "~"),
            Token::Colon => write!(f, ":"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::NewLine => write!(f, "line break"),
            Token::Ident(s) => write!(f, "{}", s),
            Token::And => write!(f, " and "),
            Token::Or => write!(f, " or "),
            Token::Not => write!(f, " not "),
            Token::Kwd(k) => write!(f, "{}", k),
            Token::String(s) => {
                write!(f, "\"")?;
                for c in s {
                    write!(f, "{}", *c as char)?;
                }
                write!(f, "\"")
            }
            Token::LessEqual => write!(f, "<="),
            Token::Less => write!(f, "<"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Greater => write!(f, ">"),
            Token::EqualEqual => write!(f, "=="),
            Token::NotEqual => write!(f, "<>"),
            Token::AddEqual => write!(f, "+="),
            Token::SubEqual => write!(f, "-="),
            Token::MulEqual => write!(f, "*="),
            Token::DivEqual => write!(f, "/="),
            Token::BitAndEqual => write!(f, "&="),
            Token::BitOrEqual => write!(f, "|="),
            Token::BitXorEqual => write!(f, "^="),
            Token::Unknown(u) => write!(f, "{}", *u as char),
        }
    }
}
