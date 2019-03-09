pub const INTEGER_OBJ: &str = "INTEGER";
pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const NULL_OBJ: &str = "NULL";
pub const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";

pub const TRUE: Boolean = Boolean { value: true };
pub const FALSE: Boolean = Boolean { value: false };
pub const NULL: Null = Null {};

#[derive(Debug)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    ReturnValue(Box<ReturnValue>),
}

pub trait ObjectVariant {
    fn kind(&self) -> &str;
    fn inspect(&self) -> String;
}

impl ObjectVariant for Object {
    fn kind(&self) -> &str {
        match self {
            Object::Integer(int) => int.kind(),
            Object::Boolean(boolean) => boolean.kind(),
            Object::Null(null) => null.kind(),
            Object::ReturnValue(r_v) => r_v.kind(),
        }
    }

    fn inspect(&self) -> String {
        match self {
            Object::Integer(int) => int.inspect(),
            Object::Boolean(boolean) => boolean.inspect(),
            Object::Null(null) => null.inspect(),
            Object::ReturnValue(return_value) => return_value.inspect(),
        }
    }
}

#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}

impl ObjectVariant for Integer {
    fn kind(&self) -> &str {
        INTEGER_OBJ
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Boolean {
    pub value: bool,
}

impl ObjectVariant for Boolean {
    fn kind(&self) -> &str {
        BOOLEAN_OBJ
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Null {}

impl ObjectVariant for Null {
    fn kind(&self) -> &str {
        NULL_OBJ
    }

    fn inspect(&self) -> String {
        String::from("null")
    }
}

#[derive(Debug)]
pub struct ReturnValue {
    pub value: Object,
}

impl ObjectVariant for ReturnValue {
    fn kind(&self) -> &str {
        NULL_OBJ
    }

    fn inspect(&self) -> String {
        format!("{}", self.value.inspect())
    }
}