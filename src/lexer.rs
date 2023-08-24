#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Illegal,
    EOF,
    Ident(&'a [u8]),
    Int(&'a [u8]),

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
    Function,
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
            Self::Ident(ident) => f.write_fmt(format_args!(
                "Ident({})",
                String::from_utf8(ident.to_vec()).unwrap()
            )),
            Self::Int(ident) => f.write_fmt(format_args!(
                "Ident({})",
                String::from_utf8(ident.to_vec()).unwrap()
            )),
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
        self.read_position = self.read_position + 1;
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
        loop {
            match self.ch {
                Some(c) => match c {
                    (b'0'..=b'9') | (b'a'..=b'z') | (b'A'..=b'Z') | b'_' => (),
                    _ => break,
                },
                None => break,
            }
            self.read_char();
        }
        &self.input[pos_start..self.position]
    }

    fn read_number(&mut self) -> &'a [u8] {
        let pos_start = self.position;
        loop {
            match self.ch {
                Some(c) => match c {
                    (b'0'..=b'9') => (),
                    _ => break,
                },
                None => break,
            }
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
            None => EOF,
            Some(c) => match c {
                (b'a'..=b'z') | (b'A'..=b'Z') | b'_' => {
                    let ident = self.read_identifier();
                    return match ident {
                        b"fn" => Function,
                        b"let" => Let,
                        b"true" => True,
                        b"false" => False,
                        b"if" => If,
                        b"else" => Else,
                        b"return" => Return,
                        _ => Ident(ident),
                    };
                }
                (b'0'..=b'9') => {
                    return Int(self.read_number());
                }
                _ => Illegal,
            },
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
            Token::EOF => None,
            _ => Some(tok),
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer { input: input }
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
            Let, Ident(b"five"), Assign, Int(b"5"), Semicolon,
            Let, Ident(b"ten"), Assign, Int(b"10"), Semicolon,
            Let, Ident(b"add"), Assign, Function, LParen, Ident(b"x"), Comma, Ident(b"y"), RParen, LBrace,
            Ident(b"x"), Plus, Ident(b"y"), Semicolon,
            RBrace, Semicolon,
            Let, Ident(b"result"), Assign, Ident(b"add"), LParen, Ident(b"five"), Comma, Ident(b"ten"), RParen, Semicolon
        };

        let lexer = Lexer::new(source);
        assert_eq!(lexer.tokens().collect::<Vec<Token>>(), expected);
    }
}
