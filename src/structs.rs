// syntax nodes
#[derive(Debug)]
pub enum Node {
    IntLiteral(i32),
    // single argument, body
    Function(String, Box<Node>),
    // variable name
    Variable(String),
    // function, argument
    Call(Box<Node>, Box<Node>),
    // name, value, rest
    Let(String, Box<Node>, Box<Node>),
}

impl Node {
    // helpers, thank goodness
    pub fn int(n: i32) -> Node {
        Node::IntLiteral(n)
    }

    pub fn fun(arg: &str, body: Node) -> Node {
        Node::Function(arg.to_string(), Box::new(body))
    }

    pub fn var(name: &str) -> Node {
        Node::Variable(name.to_string())
    }

    pub fn call(fun: Node, arg: Node) -> Node {
        Node::Call(Box::new(fun), Box::new(arg))
    }

    // had to rename argh
    pub fn set(name: &str, value: Node, rest: Node) -> Node {
        Node::Let(name.to_string(), Box::new(value), Box::new(rest))
    }

    pub fn to_string(&self) -> String {
        match self {
            Node::IntLiteral(n) => format!("{n}"),
            Node::Function(arg, body) => format!("{} => ({})", arg, body.to_string()),
            Node::Variable(name) => name.clone(),
            Node::Call(fun, arg) => format!("{}({})", fun.to_string(), arg.to_string()),
            Node::Let(name, value, rest) => {
                format!("{} = {}\n{}", name, value.to_string(), rest.to_string())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    // the id in the TypeChecker
    Abstract(usize),
    // Int is ("Int", [])
    // Result[Int, String] is ("Result", [(...), (...)])
    Concrete(String, Vec<Type>),
}

impl Type {
    pub fn function(from: Type, to: Type) -> Type {
        Type::Concrete("->".to_string(), vec![from, to])
    }

    pub fn int() -> Type {
        Type::Concrete("Int".to_string(), vec![])
    }

    pub fn bool() -> Type {
        Type::Concrete("Bool".to_string(), vec![])
    }

    // TypeChecker provides a better version
    pub fn to_string(&self) -> String {
        match self {
            Type::Abstract(id) => format!("${id}"),
            Type::Concrete(name, args) => {
                if name == "->" && args.len() == 2 {
                    format!("{} -> {}", args[0].to_string(), args[1].to_string())
                } else if args.len() == 0 {
                    name.clone()
                } else {
                    let strings: Vec<String> = args.iter().map(|t| t.to_string()).collect();
                    let joined = strings.join(", ");
                    format!("{}[{}]", name, joined)
                }
            }
        }
    }
}
