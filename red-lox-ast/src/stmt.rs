use crate::{expr::Expr, scanner::TokenWithLocation};

#[derive(Clone)]
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
    Var(TokenWithLocation, Option<Box<Expr>>),
    Block(Vec<Box<Stmt>>),
    Function {
        name: TokenWithLocation,
        params: Vec<TokenWithLocation>,
        body: Vec<Box<Stmt>>,
    },
    Class {
        name: TokenWithLocation,
        methods: Vec<Box<Stmt>>,
        superclass: Option<Box<Expr>>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    Break,
    Return(TokenWithLocation, Option<Box<Expr>>),
}

//impl Stmt {
//    pub fn find_method<'a, 'b>(&self, name: &'b str) -> Option<&'a Box<Stmt>> {
//        use Stmt::*;
//        let mut cls = self;
//        loop {
//            match cls {
//                Class {
//                    name: _,
//                    methods,
//                    superclass,
//                } => {
//                    if let Some(m) = methods.get(name) {
//
//                    }
//                }
//                _ => unreachable!(),
//            }
//        }
//    }
//}
