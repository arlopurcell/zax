
pub struct Lexer<'a> {
    source: &'a[u8],
    start: usize,
    current: usize,
    line: u32,
}

pub struct Token<'a> {
    pub tok_type: TokenType,
    pub source: &'a str,
    pub line: u32,
}

impl <'a> Token<'a>  {
    fn new(tok_type: TokenType, source: &'a str, line: u32) -> Self {
        Self {tok_type, source, line}
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus,
    SemiColon, Slash, Star,

    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    Identifier, Str, Integer, Float,

    And, Else, False,
    For, Fun, If, In, Or,
    Print,
    Return, True, Let, While,

    Error,
    Eof,
}

impl <'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn next(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;
        if self.is_at_end() {
            self.make_token(TokenType::Eof)
        } else {
            match self.advance() {
                '(' => self.make_token(TokenType::LeftParen),
                ')' => self.make_token(TokenType::RightParen),
                '{' => self.make_token(TokenType::LeftBrace),
                '}' => self.make_token(TokenType::RightBrace),
                ';' => self.make_token(TokenType::SemiColon),
                ',' => self.make_token(TokenType::Comma),
                '.' => self.make_token(TokenType::Dot),
                '-' => self.make_token(TokenType::Minus),
                '+' => self.make_token(TokenType::Plus),
                '/' => self.make_token(TokenType::Slash),
                '*' => self.make_token(TokenType::Star),
                '!' => if self.match_char('=') { self.make_token(TokenType::BangEqual) } else { self.make_token(TokenType::Bang) },
                '=' => if self.match_char('=') { self.make_token(TokenType::EqualEqual) } else { self.make_token(TokenType::Equal) },
                '<' => if self.match_char('=') { self.make_token(TokenType::LessEqual) } else { self.make_token(TokenType::Less) },
                '>' => if self.match_char('=') { self.make_token(TokenType::GreaterEqual) } else { self.make_token(TokenType::Greater) },
                '"' => self.string(),
                c => {
                    if c.is_digit(10) {
                        self.number()
                    } else if c.is_alphabetic() {
                        self.identifier()
                    } else {
                        self.error_token("Unexpected character.")
                    }
                }
            }
        }
    }

    fn skip_whitespace(&mut self) -> () {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {self.current += 1},
                '\n' => {
                    self.current += 1;
                    self.line += 1;
                },
                '/' => if self.peek_2() == '/' {
                    // comment until the end of the line
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.current += 1;
                    }
                } else {
                    break;
                },
                _ => {break;}
            }
        }
    }

    fn peek(&self) -> char {
        if self.current >= self.source.len() {
            '\0'
        } else {
            self.source[self.current].into()
        }
    }
    
    fn peek_2(&self) -> char {
        if self.is_at_end() || self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1].into()
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len() - 1
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1].into()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            false
        } else if expected != self.source[self.current].into() {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn make_token(&self, tok_type: TokenType) -> Token {
        Token::new(tok_type, &std::str::from_utf8(&self.source[self.start..self.current]).unwrap(), self.line)
    }

    fn error_token(&self, message: &'static str) -> Token {
        Token::new(TokenType::Error, message, self.line)
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.current += 1;
        }

        if self.is_at_end() {
            self.error_token("Unterminated string.")
        } else {
            self.current += 1; // consume closing quote
            self.make_token(TokenType::Str)
        }
    }

    fn number(&mut self) -> Token {
        while self.peek().is_digit(10) {
            self.current += 1;
        }

        // TODO handle 1e6 and 1.32e9 floats
        if self.peek() == '.' && self.peek_2().is_digit(10) {
            self.current += 1; // consume '.'
            while self.peek().is_digit(10) {
                self.current += 1;
            }
            self.make_token(TokenType::Float)
        } else {
            self.make_token(TokenType::Integer)
        }
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() {
            self.current += 1;
        }
        self.make_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        match char::from(self.source[self.start]) {
            'a' => self.check_keyword(1, "nd", TokenType::And),
            'e' => self.check_keyword(1, "lse", TokenType::Else),
            'l' => self.check_keyword(1, "et", TokenType::Let),
            'o' => self.check_keyword(1, "r", TokenType::Or),
            'p' => self.check_keyword(1, "rint", TokenType::Print),
            'r' => self.check_keyword(1, "eturn", TokenType::Return),
            't' => self.check_keyword(1, "rue", TokenType::True),
            'w' => self.check_keyword(1, "hile", TokenType::While),
            'i' => if self.current - self.start > 1 {
                match char::from(self.source[self.start + 1]) {
                    'f' => self.check_keyword(2, "", TokenType::If),
                    'n' => self.check_keyword(2, "", TokenType::In),
                    _ => TokenType::Identifier,
                }
            } else {
                TokenType::Identifier
            },
            'f' => if self.current - self.start > 1 {
                match char::from(self.source[self.start + 1]) {
                    'a' => self.check_keyword(2, "lse", TokenType::False),
                    'o' => self.check_keyword(2, "r", TokenType::For),
                    'u' => self.check_keyword(2, "un", TokenType::Fun),
                    _ => TokenType::Identifier,
                }
            } else {
                TokenType::Identifier
            },
            _ => TokenType::Identifier,
        }
    }

    fn check_keyword(&self, start: usize, rest: &'static str, tok_type: TokenType) -> TokenType {
        if std::str::from_utf8(&self.source[self.start + start..self.start + start + rest.len()]).unwrap() == rest {
            tok_type
        } else {
            TokenType::Identifier
        }
    }
}




