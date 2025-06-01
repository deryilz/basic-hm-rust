mod structs;

use structs::{Node, Type};

type Env = im::HashMap<String, Type>;

struct TypeChecker {
    // Type ids index into this
    types: Vec<Option<Type>>,
}

impl TypeChecker {
    fn new() -> TypeChecker {
        TypeChecker { types: vec![] }
    }

    fn new_abstract(&mut self) -> Type {
        let next_id = self.types.len();
        self.types.push(None);
        Type::Abstract(next_id)
    }

    fn analyze(&mut self, node: Node, env: &Env, non_gen: &[usize]) -> Type {
        match node {
            Node::IntLiteral(_) => Type::int(),
            Node::Variable(name) => match env.get(&name) {
                Some(var_type) => self.fresh(var_type.clone(), non_gen),
                None => panic!("Symbol not found: {}", name),
            },
            Node::Call(fun, arg) => {
                let fun_type = self.analyze(*fun, &env, non_gen);
                let arg_type = self.analyze(*arg, &env, non_gen);
                let out_type = self.new_abstract();
                self.unify(Type::function(arg_type, out_type.clone()), fun_type);
                out_type
            }
            Node::Function(arg, body) => {
                let arg_type = self.new_abstract();
                let new_env = env.update(arg.clone(), arg_type.clone());
                let mut new_non_gen = non_gen.to_vec();
                new_non_gen.push(arg_type.id());
                let out_type = self.analyze(*body, &new_env, &new_non_gen);
                Type::function(arg_type, out_type)
            }
            Node::Let(name, value, rest) => {
                let var_type = self.analyze(*value, env, non_gen);
                let new_env = env.update(name.clone(), var_type.clone());
                self.analyze(*rest, &new_env, non_gen)
            }
        }
    }

    fn fresh(&self, ty: Type, non_gen: &[usize]) -> Type {
        // TODO: actually figure out what this does
        // and implement from https://dysphoria.net/code/hindley-milner/HindleyMilner.scala
        ty
    }

    fn unify(&mut self, t1: Type, t2: Type) {
        let t1 = self.prune(t1);
        let t2 = self.prune(t2);
        if t1 == t2 {
            return;
        }

        match (t1, t2) {
            (Type::Abstract(id), t2) => {
                if self.types_overlap(id, &t2) {
                    panic!("Recursive unification is bad!")
                }

                // where the magic happens
                self.types[id] = Some(t2);
            }
            (Type::Concrete(name1, args1), Type::Concrete(name2, args2)) => {
                if name1 != name2 || args1.len() != args2.len() {
                    // TODO: more descriptive error message
                    panic!("Types don't match.")
                }

                for (a1, a2) in args1.into_iter().zip(args2.into_iter()) {
                    self.unify(a1, a2);
                }
            }
            // at this point, must be (Concrete, Abstract)
            (t1, t2) => self.unify(t2, t1),
        }
    }

    fn prune(&mut self, ty: Type) -> Type {
        if let Type::Abstract(id) = ty {
            if let Some(inst) = self.types[id].take() {
                let pruned = self.prune(inst);
                self.types[id] = Some(pruned.clone());
                return pruned;
            }
        }

        ty
    }

    fn types_overlap(&mut self, id: usize, t2: &Type) -> bool {
        let pruned = self.prune(t2.clone());
        if pruned == Type::Abstract(id) {
            true
        } else if let Type::Concrete(_, args) = t2 {
            args.iter().any(|t| self.types_overlap(id, t))
        } else {
            false
        }
    }
}

fn main() {
    use Node::*;
    // let term = Function("x".to_string(), Box::new(Variable("x".to_string())));
    let term = Function(
        "x".to_string(),
        Box::new(Function(
            "y".to_string(),
            Box::new(Variable("x".to_string())),
        )),
    );
    // let term = IntLiteral(3);
    let env = im::HashMap::new();

    let mut checker = TypeChecker::new();
    let t = checker.analyze(term, &env, &[]);
    println!("{}", t.to_string());

    for t in checker.types {
        println!("{:?}", t);
    }
}
