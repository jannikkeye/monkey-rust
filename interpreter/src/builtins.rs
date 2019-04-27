use crate::object::{Object, STRING_OBJ, BuiltInFunction};

fn len(args: &[Option<Object>]) -> Object {
    if args.len() != 1  {
        return Object::Error(format!("Wrong number of arguments supplied. Expected 1, found {}.", args.len()));
    }

    if let Some(Object::Str(string)) = &args[0] {
        return Object::Integer(string.len() as i64);
    }

    return Object::Error(format!("Wrong object variant supplied. Expected {}, found {}.", STRING_OBJ, args[0].as_ref().unwrap().kind()));
}

fn reverse(args: &[Option<Object>]) -> Object {
    if args.len() != 1  {
        return Object::Error(format!("Wrong number of arguments supplied. Expected 1, found {}.", args.len()));
    }

    if let Some(Object::Str(string)) = &args[0] {
        return Object::Str(string.chars().rev().collect::<String>());
    }

    return Object::Error(format!("Wrong object variant supplied. Expected {}, found {}.", STRING_OBJ, args[0].as_ref().unwrap().kind()));
}

pub const BUILTINS: &'static [(&'static str, Object)] = &[
    ("len", Object::BuiltInFunction(BuiltInFunction { func: len })),
    ("reverse", Object::BuiltInFunction(BuiltInFunction { func: reverse })),
];