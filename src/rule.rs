use chrono::{self, Utc};
use log::{error, warn};
use regex::Regex;
use serde::Deserialize;

use crate::store::Store;
use crate::user::{AttributeValue, User};
use crate::variation::VariationOrRollout;

#[derive(Clone, Debug, Deserialize)]
pub struct Clause {
    pub attribute: String,
    pub negate: bool,
    pub op: Op,
    pub values: Vec<AttributeValue>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct FlagRule {
    pub clauses: Vec<Clause>,
    #[serde(flatten)]
    pub variation_or_rollout: VariationOrRollout,
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum Op {
    In,
    StartsWith,
    EndsWith,
    Contains,
    Matches,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Before,
    After,
    SegmentMatch,
    SemVerEqual,
    SemVerGreaterThan,
    SemVerLessThan,
}

impl Clause {
    fn matches(&self, user: &User, store: &dyn Store) -> bool {
        if let Op::SegmentMatch = self.op {
            self.matches_segment(user, store)
        } else {
            self.matches_non_segment(user)
        }
    }

    fn maybe_negate(&self, v: bool) -> bool {
        if self.negate {
            !v
        } else {
            v
        }
    }

    pub(crate) fn matches_segment(&self, user: &User, store: &dyn Store) -> bool {
        let any_match = self.values.iter().find(|value| {
            value
                .as_str()
                .and_then(|segment_key| store.segment(segment_key))
                .map(|segment| segment.contains(user))
                .unwrap_or(false)
        });
        self.maybe_negate(any_match.is_some())
    }

    pub(crate) fn matches_non_segment(&self, user: &User) -> bool {
        let user_val = match user.value_of(&self.attribute) {
            Some(v) => v,
            None => return false,
        };

        let any_match = user_val.find(|user_val_v| {
            let any_match_for_v = self
                .values
                .iter()
                .find(|clause_val| self.op.matches(user_val_v, clause_val));
            any_match_for_v.is_some()
        });

        self.maybe_negate(any_match.is_some())
    }
}

impl FlagRule {
    pub fn matches(&self, user: &User, store: &dyn Store) -> bool {
        // rules match if _all_ of their clauses do
        for clause in &self.clauses {
            if !clause.matches(user, store) {
                return false;
            }
        }
        true
    }
}

impl Op {
    fn matches(&self, lhs: &AttributeValue, rhs: &AttributeValue) -> bool {
        match self {
            Op::In => lhs == rhs,

            // string ops
            Op::StartsWith => string_op(lhs, rhs, |l, r| l.starts_with(r)),
            Op::EndsWith => string_op(lhs, rhs, |l, r| l.ends_with(r)),
            Op::Contains => string_op(lhs, rhs, |l, r| l.find(r).is_some()),
            Op::Matches => string_op(lhs, rhs, |l, r| match Regex::new(r) {
                Ok(re) => re.is_match(l),
                Err(e) => {
                    warn!("Invalid regex for 'matches' operator ({}): {}", e, l);
                    false
                }
            }),

            // numeric ops
            Op::LessThan => numeric_op(lhs, rhs, |l, r| l < r),
            Op::LessThanOrEqual => numeric_op(lhs, rhs, |l, r| l <= r),
            Op::GreaterThan => numeric_op(lhs, rhs, |l, r| l > r),
            Op::GreaterThanOrEqual => numeric_op(lhs, rhs, |l, r| l >= r),

            Op::Before => time_op(lhs, rhs, |l, r| l < r),
            Op::After => time_op(lhs, rhs, |l, r| l > r),

            Op::SegmentMatch => {
                error!("segmentMatch operator should be special-cased, shouldn't get here");
                false
            }

            // TODO test semver ops
            Op::SemVerEqual => semver_op(lhs, rhs, |l, r| l == r),
            Op::SemVerLessThan => semver_op(lhs, rhs, |l, r| l < r),
            Op::SemVerGreaterThan => semver_op(lhs, rhs, |l, r| l > r),
        }
    }
}

fn string_op<F: Fn(&String, &String) -> bool>(
    lhs: &AttributeValue,
    rhs: &AttributeValue,
    f: F,
) -> bool {
    match (lhs.as_str(), rhs.as_str()) {
        (Some(l), Some(r)) => f(l, r),
        _ => false,
    }
}

fn numeric_op<F: Fn(f64, f64) -> bool>(lhs: &AttributeValue, rhs: &AttributeValue, f: F) -> bool {
    match (lhs.to_f64(), rhs.to_f64()) {
        (Some(l), Some(r)) => f(l, r),
        _ => false,
    }
}

fn time_op<F: Fn(chrono::DateTime<Utc>, chrono::DateTime<Utc>) -> bool>(
    lhs: &AttributeValue,
    rhs: &AttributeValue,
    f: F,
) -> bool {
    match (lhs.to_datetime(), rhs.to_datetime()) {
        (Some(l), Some(r)) => f(l, r),
        _ => false,
    }
}

fn semver_op<F: Fn(semver::Version, semver::Version) -> bool>(
    lhs: &AttributeValue,
    rhs: &AttributeValue,
    f: F,
) -> bool {
    match (lhs.as_semver(), rhs.as_semver()) {
        (Some(l), Some(r)) => f(l, r),
        _ => false,
    }
}
