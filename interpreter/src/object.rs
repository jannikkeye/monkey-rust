use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::ast::identifier::Identifier;
use crate::ast::statement::BlockStatement;
use crate::environment::Environment;

pub const INTEGER_OBJ: &'static str = "INTEGER";
pub const BOOLEAN_OBJ: &'static str = "BOOLEAN";
pub const NULL_OBJ: &'static str = "NULL";
pub const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";
pub const ERROR_OBJ: &'static str = "ERROR";
pub const FUNCTION_OBJ: &'static str = "FUNCTION";
pub const STRING_OBJ : &'static str = "STRING";
pub const BUILTIN_OBJ: &'static str = "BUILTIN";

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Object {
    Integer(i64),
    Str(String),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function(Vec<Identifier>, BlockStatement, Rc<RefCell<Environment>>),
    BuiltInFunction(Vec<Identifier>),
}

impl Object {
    pub fn is_error(&self) -> bool {
        self.kind() == ERROR_OBJ
    }

    pub fn is_null(&self) -> bool {
        self.kind() == NULL_OBJ
    }

    pub fn is_integer(&self) -> bool {
        self.kind() == INTEGER_OBJ
    }

    pub fn is_boolean(&self) -> bool {
        self.kind() == BOOLEAN_OBJ
    }

    pub fn is_return_value(&self) -> bool {
        self.kind() == RETURN_VALUE_OBJ
    }

    pub fn is_function(&self) -> bool {
        self.kind() == FUNCTION_OBJ
    }

    pub fn is_string(&self) -> bool {
        self.kind() == STRING_OBJ
    }

    pub fn kind(&self) -> &str {
        match self {
            Object::Integer(_) => INTEGER_OBJ,
            Object::Boolean(_) => BOOLEAN_OBJ,
            Object::Null => NULL_OBJ,
            Object::ReturnValue(_) => RETURN_VALUE_OBJ,
            Object::Error(_) => ERROR_OBJ,
            Object::Function(_, _, _) => FUNCTION_OBJ,
            Object::Str(_) => STRING_OBJ,
            Object::BuiltInFunction(_) => BUILTIN_OBJ,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Object::Integer(int) => int.to_string(),
                Object::Boolean(boolean) => boolean.to_string(),
                Object::Null => "null".to_owned(),
                Object::ReturnValue(return_value) => return_value.to_string(),
                Object::Error(error) => error.to_string(),
                Object::Function(parameters, body, _) => {
                    let mut string = String::new();

                    let string_params: Vec<String> = parameters
                        .iter()
                        .map(std::string::ToString::to_string)
                        .collect();

                    string.push_str("fn");
                    string.push_str("(");
                    string.push_str(&string_params.join(", "));
                    string.push_str(") {\n");
                    string.push_str(&format!("{}", body));
                    string.push_str("\n}");

                    string
                },
                Object::Str(s) => s.to_string(),
                Object::BuiltInFunction(_) => format!("{}", "builtin function"),
            }
        )
    }
}
