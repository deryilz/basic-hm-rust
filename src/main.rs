mod structs;

use structs::{Node, Type};

type Env = im::HashMap<String, Type>;
type NewIdMap = std::collections::HashMap<usize, Type>;

struct TypeChecker {
    // Type ids index into this
    types: Vec<Option<Type>>,
}

impl TypeChecker {
    fn new() -> TypeChecker {
        TypeChecker { types: vec![] }
    }

    fn next_id(&mut self) -> usize {
        self.types.len()
    }

    fn new_abstract(&mut self) -> Type {
        let id = self.next_id();
        self.types.push(None);
        Type::Abstract(id)
    }

    fn analyze(&mut self, node: Node, env: &Env, non_gen: &[usize]) -> Type {
        // println!("-- CALL analyze");
        // println!("{:?}", node);
        // println!("{:?}", env);
        // println!("{:?}", non_gen);
        match node {
            Node::BoolLiteral(_) => Type::bool(),
            Node::IntLiteral(_) => Type::int(),
            Node::Variable(name) => match env.get(&name) {
                Some(var_type) => self.fresh(var_type.clone(), non_gen, &mut NewIdMap::new()),
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
                let arg_type_id = self.next_id();
                let arg_type = self.new_abstract();

                let new_env = env.update(arg.clone(), arg_type.clone());

                let mut new_non_gen = non_gen.to_vec();
                new_non_gen.push(arg_type_id);

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

    // basically clones the type. new_ids is blank to begin with
    fn fresh(&mut self, ty: Type, non_gen: &[usize], new_ids: &mut NewIdMap) -> Type {
        // println!("-- CALL fresh");
        // println!("{:?}", ty);
        // println!("{:?}", non_gen);
        // println!("{:?}", new_ids);
        match self.prune(ty) {
            Type::Abstract(id) => {
                if self.type_still_generic(id, non_gen) {
                    new_ids
                        .entry(id)
                        .or_insert_with(|| self.new_abstract())
                        .clone()
                } else {
                    Type::Abstract(id)
                }
            }
            Type::Concrete(name, args) => {
                let mapped = args
                    .into_iter()
                    .map(|t| self.fresh(t, non_gen, new_ids))
                    .collect();
                Type::Concrete(name, mapped)
            }
        }
    }

    fn unify(&mut self, t1: Type, t2: Type) {
        // println!("-- CALL unify");
        // println!("{:?}", t1);
        // println!("{:?}", t2);
        let t1 = self.prune(t1);
        let t2 = self.prune(t2);
        if t1 == t2 {
            return;
        }

        match (t1.clone(), t2.clone()) {
            (Type::Abstract(id), _) => {
                if self.type_overlaps(id, &t2) {
                    panic!("Recursive unification is bad!")
                }

                // where the magic happens
                self.types[id] = Some(t2);
            }
            (Type::Concrete(name1, args1), Type::Concrete(name2, args2)) => {
                if name1 != name2 || args1.len() != args2.len() {
                    panic!(
                        "Types don't match: {} != {}",
                        t1.to_string(),
                        t2.to_string()
                    )
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
        // println!("-- CALL prune");
        // println!("{:?}", ty);

        if let Type::Abstract(id) = ty {
            if let Some(inst) = self.types[id].clone() {
                let pruned = self.prune(inst);
                self.types[id] = Some(pruned.clone());
                return pruned;
            }
        }

        ty
    }

    fn type_overlaps(&mut self, id: usize, t2: &Type) -> bool {
        let pruned = self.prune(t2.clone());
        if pruned == Type::Abstract(id) {
            true
        } else if let Type::Concrete(_, args) = t2 {
            self.type_overlaps_any(id, args)
        } else {
            false
        }
    }

    fn type_overlaps_any(&mut self, id: usize, types: &[Type]) -> bool {
        types.iter().any(|t| self.type_overlaps(id, t))
    }

    fn type_still_generic(&mut self, id: usize, types: &[usize]) -> bool {
        types
            .iter()
            .all(|t| !self.type_overlaps(id, &Type::Abstract(*t)))
    }

    // useful for printing
    fn simplified(&self, ty: Type) -> Type {
        match ty {
            Type::Abstract(id) => match &self.types[id] {
                Some(other) => self.simplified(other.clone()),
                _ => Type::Abstract(id),
            },
            Type::Concrete(name, args) => {
                let simplified = args.into_iter().map(|t| self.simplified(t)).collect();
                Type::Concrete(name, simplified)
            }
        }
    }
}

fn main() {
    let mut checker = TypeChecker::new();

    let t1 = checker.new_abstract();
    let mut env = im::HashMap::new();

    env.insert(
        "+".to_string(),
        Type::function(t1.clone(), Type::function(t1.clone(), t1)),
    );

    use Node as N;
    let term = N::fun("x", N::var("+").call(N::var("x")).call(N::int(3))).call(N::int(3));
    // let term = N::fun(
    //     "x",
    //     N::fun(
    //         "y",
    //         N::call(N::call(N::var("+"), N::var("x")), N::var("y")),
    //     ),
    // );
    // let term = N::int(3);

    let result = checker.analyze(term, &env, &[]);
    let result = checker.simplified(result);
    println!("{}", result.to_string());

    println!("All types:");
    for (i, ty) in checker.types.iter().enumerate() {
        println!("{} -> {:?}", i, ty);
    }
}
