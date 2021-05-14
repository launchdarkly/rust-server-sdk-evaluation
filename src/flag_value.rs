use log::warn;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(untagged)]
pub enum FlagValue {
    Bool(bool),
    Str(String),
    Float(f64),
    Int(i64),
    Json(serde_json::Value),
}

impl From<bool> for FlagValue {
    fn from(b: bool) -> FlagValue {
        FlagValue::Bool(b)
    }
}

impl From<String> for FlagValue {
    fn from(s: String) -> FlagValue {
        FlagValue::Str(s)
    }
}

impl From<f64> for FlagValue {
    fn from(f: f64) -> FlagValue {
        FlagValue::Float(f)
    }
}

impl From<i64> for FlagValue {
    fn from(i: i64) -> FlagValue {
        FlagValue::Int(i)
    }
}

impl From<serde_json::Value> for FlagValue {
    fn from(v: serde_json::Value) -> Self {
        use serde_json::Value;
        match v {
            Value::Bool(b) => b.into(),
            Value::Number(n) => {
                if let Some(f) = n.as_f64() {
                    f.into()
                } else if let Some(i) = n.as_i64() {
                    i.into()
                } else {
                    warn!("unrepresentable number {}, converting to string", n);
                    FlagValue::Json(format!("{}", n).into())
                }
            }
            Value::String(s) => s.into(),
            Value::Null | Value::Object(_) | Value::Array(_) => FlagValue::Json(v),
        }
    }
}

// Converting float to int has undefined behaviour for huge floats: https://stackoverflow.com/a/41139453.
// To avoid this, refuse to convert floats with magnitude greater than 2**53 - 1, after which 64-bit floats no longer
// retain integer precision. We could go a few orders of magnitude higher without triggering the UB, but this seems like
// the least surprising place to put a breakpoint.
const FLOAT_TO_INT_MAX: f64 = 9007199254740991_f64;

impl FlagValue {
    // TODO implement type coercion here?

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            FlagValue::Bool(b) => Some(*b),
            _ => {
                warn!("variation type is not bool but {:?}", self);
                None
            }
        }
    }

    pub fn as_string(&self) -> Option<String> {
        match self {
            FlagValue::Str(s) => Some(s.clone()),
            _ => {
                warn!("variation type is not str but {:?}", self);
                None
            }
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            FlagValue::Float(f) => Some(*f),
            _ => {
                warn!("variation type is not float but {:?}", self);
                None
            }
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            FlagValue::Int(i) => Some(*i),
            FlagValue::Float(f) if f.abs() <= FLOAT_TO_INT_MAX => Some(*f as i64),
            _ => None,
        }
    }

    pub fn as_json(&self) -> Option<serde_json::Value> {
        use serde_json::Value;
        match self {
            FlagValue::Bool(b) => Some(Value::from(*b)),
            FlagValue::Str(s) => Some(Value::from(s.as_str())),
            FlagValue::Float(f) => Some(Value::from(*f)),
            FlagValue::Int(i) => Some(Value::from(*i)),
            FlagValue::Json(v) => Some(v.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use spectral::prelude::*;

    #[test]
    fn float_bounds() {
        let test_cases = vec![
            (1.99, Some(1)),
            (9007199254740990.0, Some(9007199254740990)),
            (9007199254740991.0, Some(9007199254740991)),
            (9007199254740992.0, None),
            (-1.99, Some(-1)),
            (-9007199254740990.0, Some(-9007199254740990)),
            (-9007199254740991.0, Some(-9007199254740991)),
            (-9007199254740992.0, None),
        ];
        for (have, expect) in test_cases {
            assert_that!(FlagValue::Float(have).as_int()).is_equal_to(expect);
        }
    }
}
