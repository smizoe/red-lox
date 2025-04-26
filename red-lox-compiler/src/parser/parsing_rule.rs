use red_lox_ast::scanner::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Precedence {
    None = 0,
    Comma,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
    Highest,
}

impl Precedence {
    pub(crate) fn plusone(&self) -> Self {
        use Precedence::*;
        match self {
            None => Comma,
            Comma => Assignment,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Primary,
            Primary => Highest,
            Highest => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum NextExpressionType {
    None,
    Grouping,
    Unary,
    Binary,
    Number,
    String,
    Literal,
    Variable,
    And,
    Or,
    Call,
    Dot,
}

#[derive(Debug, Clone)]
pub(crate) struct Rule {
    pub precedence: Precedence,
    pub prefix: NextExpressionType,
    pub infix: NextExpressionType,
}

impl Default for Rule {
    fn default() -> Self {
        Self {
            precedence: Precedence::None,
            prefix: NextExpressionType::None,
            infix: NextExpressionType::None,
        }
    }
}

pub(crate) fn get_rule(token: &Token) -> Rule {
    use NextExpressionType::*;
    match token {
        Token::LeftParen => Rule {
            precedence: Precedence::Call,
            prefix: Grouping,
            infix: Call,
        },
        Token::RightParen => Rule::default(),
        Token::LeftBrace => Rule::default(),
        Token::RightBrace => Rule::default(),
        Token::Comma => Rule {
            precedence: Precedence::Comma,
            prefix: None,
            infix: Binary,
        },
        Token::Dot => Rule {
            precedence: Precedence::Call,
            prefix: None,
            infix: Dot,
        },
        Token::Minus => Rule {
            precedence: Precedence::Term,
            prefix: Unary,
            infix: Binary,
        },
        Token::Plus => Rule {
            precedence: Precedence::Term,
            prefix: None,
            infix: Binary,
        },
        Token::Colon => Rule::default(),
        Token::Question => Rule {
            precedence: Precedence::Assignment,
            prefix: None,
            infix: Binary,
        },
        Token::Semicolon => Rule::default(),
        Token::Slash => Rule {
            precedence: Precedence::Factor,
            prefix: None,
            infix: Binary,
        },
        Token::Star => Rule {
            precedence: Precedence::Factor,
            prefix: None,
            infix: Binary,
        },
        Token::Bang => Rule {
            precedence: Precedence::None,
            prefix: Unary,
            infix: None,
        },
        Token::BangEqual => Rule {
            precedence: Precedence::Equality,
            prefix: None,
            infix: Binary,
        },
        Token::Equal => Rule::default(),
        Token::EqualEqual => Rule {
            precedence: Precedence::Equality,
            prefix: None,
            infix: Binary,
        },
        Token::Greater => Rule {
            precedence: Precedence::Comparison,
            prefix: None,
            infix: Binary,
        },
        Token::GreaterEqual => Rule {
            precedence: Precedence::Comparison,
            prefix: None,
            infix: Binary,
        },
        Token::Less => Rule {
            precedence: Precedence::Comparison,
            prefix: None,
            infix: Binary,
        },
        Token::LessEqual => Rule {
            precedence: Precedence::Comparison,
            prefix: None,
            infix: Binary,
        },
        Token::Identifier(_) => Rule {
            precedence: Precedence::None,
            prefix: Variable,
            infix: None,
        },
        Token::String(_) => Rule {
            precedence: Precedence::None,
            prefix: String,
            infix: None,
        },
        Token::Number(_) => Rule {
            precedence: Precedence::None,
            prefix: Number,
            infix: None,
        },
        Token::And => Rule {
            precedence: Precedence::And,
            prefix: None,
            infix: And,
        },
        Token::Break => Rule::default(),
        Token::Case => Rule::default(),
        Token::Continue => Rule::default(),
        Token::Class => Rule::default(),
        Token::Default => Rule::default(),
        Token::Else => Rule::default(),
        Token::False => Rule {
            precedence: Precedence::None,
            prefix: Literal,
            infix: None,
        },
        Token::Fun => Rule::default(),
        Token::For => Rule::default(),
        Token::If => Rule::default(),
        Token::Nil => Rule {
            precedence: Precedence::None,
            prefix: Literal,
            infix: None,
        },
        Token::Or => Rule {
            precedence: Precedence::Or,
            prefix: None,
            infix: Or,
        },
        Token::Print => Rule::default(),
        Token::Return => Rule::default(),
        Token::Super => Rule::default(),
        Token::Switch => Rule::default(),
        Token::This => Rule::default(),
        Token::True => Rule {
            precedence: Precedence::None,
            prefix: Literal,
            infix: None,
        },
        Token::Var => Rule::default(),
        Token::While => Rule::default(),
        Token::Eof => Rule::default(),
    }
}
