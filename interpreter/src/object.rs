pub const INTEGER_OBJ: &str = "INTEGER";
pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const NULL_OBJ: &str = "NULL";

pub const TRUE: Boolean = Boolean { value: true };
pub const FALSE: Boolean = Boolean { value: false };
pub const NULL: Null = Null {};

#[derive(Debug)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
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
        }
    }

    fn inspect(&self) -> String {
        match self {
            Object::Integer(int) => int.inspect(),
            Object::Boolean(boolean) => boolean.inspect(),
            Object::Null(null) => null.inspect(),
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