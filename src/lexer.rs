use std::str;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token<'a> {
    Illegal,
    Eof,
    Ident(&'a str),
    Int(&'a str),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Fn,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(s) => f.write_fmt(format_args!("Ident({})", s)),
            Self::Int(s) => f.write_fmt(format_args!("Int({})", s)),
            _ => f.write_fmt(format_args!("{:?}", self)),
        }
    }
}

pub struct Tokens<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    ch: Option<u8>,
}

impl<'a> Tokens<'a> {
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input[self.read_position]);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<u8> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input[self.read_position])
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.unwrap_or(b'!').is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> &'a [u8] {
        let pos_start = self.position;
        while let Some(c) = self.ch {
            match c {
                (b'0'..=b'9') | (b'a'..=b'z') | (b'A'..=b'Z') | b'_' => {
                    self.read_char();
                }
                _ => break,
            }
        }
        &self.input[pos_start..self.position]
    }

    fn read_number(&mut self) -> &'a [u8] {
        let pos_start = self.position;

        while let Some(b'0'..=b'9') = self.ch {
            self.read_char();
        }

        &self.input[pos_start..self.position]
    }

    fn next_token(&mut self) -> Token<'a> {
        use Token::*;

        self.skip_whitespace();
        let token = match self.ch {
            Some(b'=') => match self.peek_char() {
                Some(b'=') => {
                    self.read_char();
                    Eq
                }
                _ => Assign,
            },
            Some(b'+') => Plus,
            Some(b'-') => Minus,
            Some(b'!') => match self.peek_char() {
                Some(b'=') => {
                    self.read_char();
                    NotEq
                }
                _ => Bang,
            },
            Some(b'*') => Asterisk,
            Some(b'<') => Lt,
            Some(b'>') => Gt,
            Some(b'/') => Slash,
            Some(b'(') => LParen,
            Some(b')') => RParen,
            Some(b'{') => LBrace,
            Some(b'}') => RBrace,
            Some(b',') => Comma,
            Some(b';') => Semicolon,
            Some(c) => match c {
                (b'a'..=b'z') | (b'A'..=b'Z') | b'_' => {
                    let ident = self.read_identifier();
                    return match ident {
                        b"fn" => Fn,
                        b"let" => Let,
                        b"true" => True,
                        b"false" => False,
                        b"if" => If,
                        b"else" => Else,
                        b"return" => Return,
                        _ => match str::from_utf8(ident) {
                            Ok(s) => Ident(s),
                            Err(_) => todo!(),
                        },
                    };
                }
                (b'0'..=b'9') => {
                    return match str::from_utf8(self.read_number()) {
                        Ok(s) => Int(s),
                        Err(_) => todo!(),
                    }
                }
                _ => Illegal,
            },
            None => Eof,
        };
        self.read_char();
        token
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        match tok {
            Token::Eof => None,
            _ => Some(tok),
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer { input }
    }

    pub fn tokens(&self) -> Tokens<'a> {
        let mut tokens = Tokens {
            input: self.input.as_bytes(),
            position: 0,
            read_position: 0,
            ch: None,
        };
        tokens.read_char();
        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;
    #[test]
    fn symbols() {
        let source = "=+-!*/(){},;";
        let expected = [
            Assign, Plus, Minus, Bang, Asterisk, Slash, LParen, RParen, LBrace, RBrace, Comma,
            Semicolon,
        ];
        let mut tokens = Lexer::new(source).tokens();
        expected
            .iter()
            .for_each(|t| assert_eq!(tokens.next_token(), *t));
    }

    #[test]
    fn read_identifier() {
        let mut tokens = Lexer::new("let ").tokens();
        assert_eq!(tokens.read_identifier(), b"let");
    }

    #[test]
    fn multichar_token() {
        Lexer::new("==").tokens().for_each(|t| assert_eq!(t, Eq));
        Lexer::new("!=").tokens().for_each(|t| assert_eq!(t, NotEq));
    }

    #[test]
    fn code() {
        let source = r#"
                        let five = 5;
                        let ten = 10;
                        let add = fn(x, y) {
                            x + y;
                        };
                        let result = add(five, ten);
                    "#;

        #[rustfmt::skip]
        let expected = vec!{
            Let, Ident("five"), Assign, Int("5"), Semicolon,
            Let, Ident("ten"), Assign, Int("10"), Semicolon,
            Let, Ident("add"), Assign, Fn, LParen, Ident("x"), Comma, Ident("y"), RParen, LBrace,
            Ident("x"), Plus, Ident("y"), Semicolon,
            RBrace, Semicolon,
            Let, Ident("result"), Assign, Ident("add"), LParen, Ident("five"), Comma, Ident("ten"), RParen, Semicolon
        };

        let lexer = Lexer::new(source);
        assert_eq!(lexer.tokens().collect::<Vec<Token>>(), expected);
    }
}
