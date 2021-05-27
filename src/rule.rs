use chrono::{self, Utc};
use log::{error, warn};
use regex::Regex;
use serde::Deserialize;

use crate::store::Store;
use crate::user::{AttributeValue, User};
use crate::variation::VariationOrRollout;

#[derive(Clone, Debug, Deserialize)]
pub struct Clause {
    attribute: String,
    negate: bool,
    op: Op,
    values: Vec<AttributeValue>,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FlagRule {
    pub id: String,
    clauses: Vec<Clause>,
    #[serde(flatten)]
    pub variation_or_rollout: VariationOrRollout,
    pub track_events: bool,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
enum Op {
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
            // null attributes always imply false, regardless of clause negation
            Some(AttributeValue::Null) | None => return false,
            Some(v) => v,
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
            Op::Contains => string_op(lhs, rhs, |l, r| l.contains(r)),
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

            Op::SemVerEqual => semver_op(lhs, rhs, |l, r| l == r),
            Op::SemVerLessThan => semver_op(lhs, rhs, |l, r| l < r),
            Op::SemVerGreaterThan => semver_op(lhs, rhs, |l, r| l > r),
        }
    }
}

fn string_op<F: Fn(&str, &str) -> bool>(lhs: &AttributeValue, rhs: &AttributeValue, f: F) -> bool {
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::time::SystemTime;

    use maplit::hashmap;

    use super::*;
    use crate::flag::Flag;
    use crate::segment::Segment;

    struct TestStore;

    impl Store for TestStore {
        fn flag(&self, _flag_key: &str) -> Option<&Flag> {
            None
        }
        fn segment(&self, _segment_key: &str) -> Option<&Segment> {
            None
        }
    }

    fn astring(s: &str) -> AttributeValue {
        AttributeValue::String(s.into())
    }
    fn afloat(f: f64) -> AttributeValue {
        AttributeValue::Float(f)
    }
    fn aint(i: i64) -> AttributeValue {
        AttributeValue::Int(i)
    }

    #[test]
    fn test_op_in() {
        // strings
        assert!(Op::In.matches(&astring("foo"), &astring("foo")));

        assert!(!Op::In.matches(&astring("foo"), &astring("bar")));
        assert!(
            !Op::In.matches(&astring("Foo"), &astring("foo")),
            "case sensitive"
        );

        // numbers
        assert!(Op::In.matches(&afloat(42.0), &afloat(42.0)));
        assert!(!Op::In.matches(&afloat(42.0), &afloat(3.0)));
        assert!(Op::In.matches(&afloat(0.0), &afloat(-0.0)));

        // arrays
        assert!(Op::In.matches(&vec![0.0].into(), &vec![0.0].into()));
        assert!(!Op::In.matches(&vec![0.0, 1.0].into(), &vec![0.0].into()));
        assert!(!Op::In.matches(&vec![0.0].into(), &vec![0.0, 1.0].into()));
        assert!(!Op::In.matches(&afloat(0.0), &vec![0.0].into()));
        assert!(!Op::In.matches(&vec![0.0].into(), &afloat(0.0)));

        // objects
        assert!(Op::In.matches(&hashmap! {"x" => 0.0}.into(), &hashmap! {"x" => 0.0}.into()));
        assert!(!Op::In.matches(
            &hashmap! {"x" => 0.0, "y" => 1.0}.into(),
            &hashmap! {"x" => 0.0}.into()
        ));
        assert!(!Op::In.matches(
            &hashmap! {"x" => 0.0}.into(),
            &hashmap! {"x" => 0.0, "y" => 1.0}.into()
        ));
        assert!(!Op::In.matches(&afloat(0.0), &hashmap! {"x" => 0.0}.into()));
        assert!(!Op::In.matches(&hashmap! {"x" => 0.0}.into(), &afloat(0.0)));
    }

    #[test]
    fn test_op_starts_with() {
        // degenerate cases
        assert!(Op::StartsWith.matches(&astring(""), &astring("")));
        assert!(Op::StartsWith.matches(&astring("a"), &astring("")));
        assert!(Op::StartsWith.matches(&astring("a"), &astring("a")));

        // test asymmetry
        assert!(Op::StartsWith.matches(&astring("food"), &astring("foo")));
        assert!(!Op::StartsWith.matches(&astring("foo"), &astring("food")));

        assert!(
            !Op::StartsWith.matches(&astring("Food"), &astring("foo")),
            "case sensitive"
        );
    }

    #[test]
    fn test_op_ends_with() {
        // degenerate cases
        assert!(Op::EndsWith.matches(&astring(""), &astring("")));
        assert!(Op::EndsWith.matches(&astring("a"), &astring("")));
        assert!(Op::EndsWith.matches(&astring("a"), &astring("a")));

        // test asymmetry
        assert!(Op::EndsWith.matches(&astring("food"), &astring("ood")));
        assert!(!Op::EndsWith.matches(&astring("ood"), &astring("food")));

        assert!(
            !Op::EndsWith.matches(&astring("FOOD"), &astring("ood")),
            "case sensitive"
        );
    }

    #[test]
    fn test_op_contains() {
        // degenerate cases
        assert!(Op::Contains.matches(&astring(""), &astring("")));
        assert!(Op::Contains.matches(&astring("a"), &astring("")));
        assert!(Op::Contains.matches(&astring("a"), &astring("a")));

        // test asymmetry
        assert!(Op::Contains.matches(&astring("food"), &astring("oo")));
        assert!(!Op::Contains.matches(&astring("oo"), &astring("food")));

        assert!(
            !Op::Contains.matches(&astring("FOOD"), &astring("oo")),
            "case sensitive"
        );
    }

    #[test]
    fn test_op_matches() {
        fn should_match(text: &str, pattern: &str) {
            assert!(
                Op::Matches.matches(&astring(text), &astring(pattern)),
                "`{}` should match `{}`",
                text,
                pattern
            );
        }

        fn should_not_match(text: &str, pattern: &str) {
            assert!(
                !Op::Matches.matches(&astring(text), &astring(pattern)),
                "`{}` should not match `{}`",
                text,
                pattern
            );
        }

        should_match("", "");
        should_match("a", "");
        should_match("a", "a");
        should_match("a", ".");
        should_match("hello world", "hello.*rld");
        should_match("hello world", "hello.*orl");
        should_match("hello world", "l+");
        should_match("hello world", "(world|planet)");

        should_not_match("", ".");
        should_not_match("", r"\");
        should_not_match("hello world", "aloha");
        should_not_match("hello world", "***bad regex");
    }

    #[test]
    fn test_ops_numeric() {
        // basic numeric comparisons
        assert!(Op::LessThan.matches(&afloat(0.0), &afloat(1.0)));
        assert!(!Op::LessThan.matches(&afloat(0.0), &afloat(0.0)));
        assert!(!Op::LessThan.matches(&afloat(1.0), &afloat(0.0)));

        assert!(Op::GreaterThan.matches(&afloat(1.0), &afloat(0.0)));
        assert!(!Op::GreaterThan.matches(&afloat(0.0), &afloat(0.0)));
        assert!(!Op::GreaterThan.matches(&afloat(0.0), &afloat(1.0)));

        assert!(Op::LessThanOrEqual.matches(&afloat(0.0), &afloat(1.0)));
        assert!(Op::LessThanOrEqual.matches(&afloat(0.0), &afloat(0.0)));
        assert!(!Op::LessThanOrEqual.matches(&afloat(1.0), &afloat(0.0)));

        assert!(Op::GreaterThanOrEqual.matches(&afloat(1.0), &afloat(0.0)));
        assert!(Op::GreaterThanOrEqual.matches(&afloat(0.0), &afloat(0.0)));
        assert!(!Op::GreaterThanOrEqual.matches(&afloat(0.0), &afloat(1.0)));

        // no conversions
        assert!(!Op::LessThan.matches(&astring("0"), &afloat(1.0)));
        assert!(!Op::LessThan.matches(&afloat(0.0), &astring("1")));
    }

    #[test]
    fn test_ops_time() {
        let today = SystemTime::now();
        let today_millis = today
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_millis() as f64;
        let yesterday_millis = today_millis - 86_400_000 as f64;

        // basic UNIX timestamp comparisons
        assert!(Op::Before.matches(&afloat(yesterday_millis), &afloat(today_millis)));
        assert!(!Op::Before.matches(&afloat(today_millis), &afloat(yesterday_millis)));
        assert!(!Op::Before.matches(&afloat(today_millis), &afloat(today_millis)));

        assert!(Op::After.matches(&afloat(today_millis), &afloat(yesterday_millis)));
        assert!(!Op::After.matches(&afloat(yesterday_millis), &afloat(today_millis)));
        assert!(!Op::After.matches(&afloat(today_millis), &afloat(today_millis)));

        // numeric strings get converted as millis
        assert!(Op::Before.matches(
            &astring(&yesterday_millis.to_string()),
            &afloat(today_millis)
        ));
        assert!(Op::After.matches(
            &afloat(today_millis),
            &astring(&yesterday_millis.to_string())
        ));

        // date-formatted strings get parsed
        assert!(Op::Before.matches(
            &astring("2019-11-19T17:29:00.000000-07:00"),
            &afloat(today_millis)
        ));
        assert!(
            Op::Before.matches(&astring("2019-11-19T17:29:00-07:00"), &afloat(today_millis)),
            "fractional seconds part is optional"
        );

        assert!(Op::After.matches(
            &afloat(today_millis),
            &astring("2019-11-19T17:29:00.000000-07:00")
        ));

        // nonsense strings don't match
        assert!(!Op::Before.matches(&astring("fish"), &afloat(today_millis)));
        assert!(!Op::After.matches(&afloat(today_millis), &astring("fish")));
    }

    #[test]
    fn test_semver_ops() {
        assert!(Op::SemVerEqual.matches(&astring("2.0.0"), &astring("2.0.0")));

        assert!(
            Op::SemVerEqual.matches(&astring("2.0"), &astring("2.0.0")),
            "we allow missing components (filled in with zeroes)"
        );
        assert!(
            Op::SemVerEqual.matches(&astring("2"), &astring("2.0.0")),
            "we allow missing components (filled in with zeroes)"
        );

        assert!(!Op::SemVerEqual.matches(&astring("2.0.0"), &astring("3.0.0")));
        assert!(!Op::SemVerEqual.matches(&astring("2.0.0"), &astring("2.1.0")));
        assert!(!Op::SemVerEqual.matches(&astring("2.0.0"), &astring("2.0.1")));

        assert!(Op::SemVerGreaterThan.matches(&astring("3.0.0"), &astring("2.0.0")));
        assert!(Op::SemVerGreaterThan.matches(&astring("2.1.0"), &astring("2.0.0")));
        assert!(Op::SemVerGreaterThan.matches(&astring("2.0.1"), &astring("2.0.0")));
        assert!(Op::SemVerGreaterThan
            .matches(&astring("2.0.0-rc.10.green"), &astring("2.0.0-rc.2.green")));
        assert!(
            Op::SemVerGreaterThan.matches(&astring("2.0.0-rc.2.red"), &astring("2.0.0-rc.2.green")),
            "red > green"
        );
        assert!(
            Op::SemVerGreaterThan
                .matches(&astring("2.0.0-rc.2.green.1"), &astring("2.0.0-rc.2.green")),
            "adding more version components makes it greater"
        );

        assert!(!Op::SemVerGreaterThan.matches(&astring("2.0.0"), &astring("2.0.0")));
        assert!(!Op::SemVerGreaterThan.matches(&astring("1.9.0"), &astring("2.0.0")));
        assert!(
            !Op::SemVerGreaterThan.matches(&astring("2.0.0-rc"), &astring("2.0.0")),
            "prerelease version < released version"
        );
        assert!(
            !Op::SemVerGreaterThan.matches(&astring("2.0.0+build"), &astring("2.0.0")),
            "build metadata is ignored, these versions are equal"
        );

        assert!(!Op::SemVerEqual.matches(&astring("2.0.0"), &astring("200")));

        // we don't convert
        assert!(!Op::SemVerEqual.matches(&astring("2.0.0"), &afloat(2.0)));
        assert!(!Op::SemVerEqual.matches(&astring("2.0.0"), &aint(2)));
    }

    #[test]
    fn test_clause_matches() {
        let one_val_clause = Clause {
            attribute: "a".into(),
            negate: false,
            op: Op::In,
            values: vec!["foo".into()],
        };
        let many_val_clause = Clause {
            attribute: "a".into(),
            negate: false,
            op: Op::In,
            values: vec!["foo".into(), "bar".into()],
        };
        let negated_clause = Clause {
            attribute: "a".into(),
            negate: true,
            op: Op::In,
            values: vec!["foo".into()],
        };
        let negated_many_val_clause = Clause {
            attribute: "a".into(),
            negate: true,
            op: Op::In,
            values: vec!["foo".into(), "bar".into()],
        };
        let key_clause = Clause {
            attribute: "key".into(),
            negate: false,
            op: Op::In,
            values: vec!["mu".into()],
        };

        let matching_user = User::with_key("mu")
            .custom(hashmap! {"a".into() => "foo".into()})
            .build();
        let non_matching_user = User::with_key("nmu")
            .custom(hashmap! {"a".into() => "lol".into()})
            .build();
        let user_without_attr = User::with_key("uwa").build();

        assert!(one_val_clause.matches(&matching_user, &TestStore {}));
        assert!(!one_val_clause.matches(&non_matching_user, &TestStore {}));
        assert!(!one_val_clause.matches(&user_without_attr, &TestStore {}));

        assert!(!negated_clause.matches(&matching_user, &TestStore {}));
        assert!(negated_clause.matches(&non_matching_user, &TestStore {}));

        assert!(
            !negated_clause.matches(&user_without_attr, &TestStore {}),
            "targeting missing attribute does not match even when negated"
        );

        assert!(
            many_val_clause.matches(&matching_user, &TestStore {}),
            "requires only one of the values"
        );
        assert!(!many_val_clause.matches(&non_matching_user, &TestStore {}));
        assert!(!many_val_clause.matches(&user_without_attr, &TestStore {}));

        assert!(
            !negated_many_val_clause.matches(&matching_user, &TestStore {}),
            "requires all values are missing"
        );
        assert!(negated_many_val_clause.matches(&non_matching_user, &TestStore {}));

        assert!(
            !negated_many_val_clause.matches(&user_without_attr, &TestStore {}),
            "targeting missing attribute does not match even when negated"
        );

        assert!(
            key_clause.matches(&matching_user, &TestStore {}),
            "should match key"
        );
        assert!(
            !key_clause.matches(&non_matching_user, &TestStore {}),
            "should not match non-matching key"
        );

        let user_with_many = User::with_key("uwm")
            .custom(hashmap! {"a".into() => vec!["foo", "bar", "lol"].into()})
            .build();

        assert!(one_val_clause.matches(&user_with_many, &TestStore {}));
        assert!(many_val_clause.matches(&user_with_many, &TestStore {}));

        assert!(!negated_clause.matches(&user_with_many, &TestStore {}));
        assert!(!negated_many_val_clause.matches(&user_with_many, &TestStore {}));
    }

    struct AttributeTestCase {
        matching_user: User,
        non_matching_user: User,
        user_without_attr: Option<User>,
    }

    #[test]
    fn test_clause_matches_attributes() {
        let tests: HashMap<&str, AttributeTestCase> = hashmap! {
            "key" => AttributeTestCase {
                matching_user: User::with_key("match").build(),
                non_matching_user: User::with_key("nope").build(),
                user_without_attr: None,
            },
            "secondary" => AttributeTestCase {
                matching_user: User::with_key("mu").secondary("match").build(),
                non_matching_user: User::with_key("nmu").secondary("nope").build(),
                user_without_attr: Some(User::with_key("uwa").build()),
            },
            "ip" => AttributeTestCase {
                matching_user: User::with_key("mu").ip("match").build(),
                non_matching_user: User::with_key("nmu").ip("nope").build(),
                user_without_attr: Some(User::with_key("uwa").build()),
            },
            "country" => AttributeTestCase {
                matching_user: User::with_key("mu").country("match").build(),
                non_matching_user: User::with_key("nmu").country("nope").build(),
                user_without_attr: Some(User::with_key("uwa").build()),
            },
            "email" => AttributeTestCase {
                matching_user: User::with_key("mu").email("match").build(),
                non_matching_user: User::with_key("nmu").email("nope").build(),
                user_without_attr: Some(User::with_key("uwa").build()),
            },
            "firstName" => AttributeTestCase {
                matching_user: User::with_key("mu").first_name("match").build(),
                non_matching_user: User::with_key("nmu").first_name("nope").build(),
                user_without_attr: Some(User::with_key("uwa").build()),
            },
            "lastName" => AttributeTestCase {
                matching_user: User::with_key("mu").last_name("match").build(),
                non_matching_user: User::with_key("nmu").last_name("nope").build(),
                user_without_attr: Some(User::with_key("uwa").build()),
            },
            "avatar" => AttributeTestCase {
                matching_user: User::with_key("mu").avatar("match").build(),
                non_matching_user: User::with_key("nmu").avatar("nope").build(),
                user_without_attr: Some(User::with_key("uwa").build()),
            },
            "name" => AttributeTestCase {
                matching_user: User::with_key("mu").name("match").build(),
                non_matching_user: User::with_key("nmu").name("nope").build(),
                user_without_attr: Some(User::with_key("uwa").build()),
            },
        };

        for (attr, test_case) in tests {
            let clause = Clause {
                attribute: attr.into(),
                negate: false,
                op: Op::In,
                values: vec!["match".into()],
            };

            assert!(
                clause.matches(&test_case.matching_user, &TestStore {}),
                "should match {}",
                attr
            );
            assert!(
                !clause.matches(&test_case.non_matching_user, &TestStore {}),
                "should not match non-matching {}",
                attr
            );
            if let Some(user_without_attr) = test_case.user_without_attr {
                assert!(
                    !clause.matches(&user_without_attr, &TestStore {}),
                    "should not match user with null {}",
                    attr
                );
            }
        }
    }

    #[test]
    fn test_clause_matches_anonymous_attribute() {
        let clause = Clause {
            attribute: "anonymous".into(),
            negate: false,
            op: Op::In,
            values: vec![true.into()],
        };

        let anon_user = User::with_key("anon").anonymous(true).build();
        let non_anon_user = User::with_key("nonanon").anonymous(false).build();
        let implicitly_non_anon_user = User::with_key("implicit").build();

        assert!(clause.matches(&anon_user, &TestStore {}));
        assert!(!clause.matches(&non_anon_user, &TestStore {}));
        assert!(!clause.matches(&implicitly_non_anon_user, &TestStore {}));
    }

    #[test]
    fn test_clause_matches_custom_attributes() {
        for attr in vec![
            "custom",  // check we can have an attribute called "custom"
            "custom1", // check custom attributes work the same
        ] {
            let clause = Clause {
                attribute: attr.into(),
                negate: false,
                op: Op::In,
                values: vec!["match".into()],
            };

            let matching_user = User::with_key("mu")
                .custom(hashmap! {attr.into() => "match".into()})
                .build();
            let non_matching_user = User::with_key("nmu")
                .custom(hashmap! {attr.into() => "nope".into()})
                .build();
            let user_without_attr = User::with_key("uwa")
                .custom(hashmap! {attr.into() => AttributeValue::Null})
                .build();

            assert!(
                clause.matches(&matching_user, &TestStore {}),
                "should match {}",
                attr
            );
            assert!(
                !clause.matches(&non_matching_user, &TestStore {}),
                "should not match non-matching {}",
                attr
            );
            assert!(
                !clause.matches(&user_without_attr, &TestStore {}),
                "should not match user with null {}",
                attr
            );
        }
    }

    #[test]
    fn test_null_attribute() {
        let user_null_attr = User::with_key("key")
            .custom(hashmap!["attr".to_string() => AttributeValue::Null])
            .build();

        let user_missing_attr = User::with_key("key").build();

        let clause_values = vec![
            AttributeValue::Bool(true),
            AttributeValue::Bool(false),
            AttributeValue::Float(1.5),
            AttributeValue::Int(1),
            AttributeValue::Null,
            AttributeValue::String("abc".to_string()),
            AttributeValue::Array(vec![
                AttributeValue::String("def".to_string()),
                AttributeValue::Null,
            ]),
        ];

        for op in &[
            Op::In,
            Op::StartsWith,
            Op::EndsWith,
            Op::Contains,
            Op::Matches,
            Op::LessThan,
            Op::LessThanOrEqual,
            Op::GreaterThan,
            Op::GreaterThanOrEqual,
            Op::Before,
            Op::After,
            Op::SemVerEqual,
            Op::SemVerGreaterThan,
            Op::SemVerLessThan,
        ] {
            for neg in &[true, false] {
                let clause = Clause {
                    attribute: "attr".to_string(),
                    negate: *neg,
                    op: *op,
                    values: clause_values.clone(),
                };
                assert!(
                    !clause.matches(&user_null_attr, &TestStore {}),
                    "Null attribute matches operator {:?} when {}negated",
                    clause.op,
                    if *neg { "" } else { "not " },
                );
                assert!(
                    !clause.matches(&user_missing_attr, &TestStore {}),
                    "Missing attribute matches operator {:?} when {}negated",
                    clause.op,
                    if *neg { "" } else { "not " },
                );
            }
        }
    }

    // The following test cases are ported from the Go implementation:
    // https://github.com/launchdarkly/go-server-sdk-evaluation/blob/v1/ldmodel/match_clause_operator_test.go#L28-L155
    fn clause_test_case<S, T>(op: Op, user_value: S, clause_value: T, expected: bool)
    where
        AttributeValue: From<S>,
        AttributeValue: From<T>,
    {
        let clause = Clause {
            attribute: "attr".to_string(),
            negate: false,
            op: op,
            values: match clause_value.into() {
                AttributeValue::Array(vec) => vec,
                other => vec![other],
            },
        };

        let user = User::with_key("key")
            .custom(hashmap!["attr".to_string() => user_value.into()])
            .build();
        assert_eq!(
            clause.matches(&user, &TestStore {}),
            expected,
            "{:?} {:?} {:?} should be {}",
            user.value_of("attr").unwrap(),
            clause.op,
            clause.values,
            &expected
        );
    }

    #[test]
    fn test_numeric_clauses() {
        clause_test_case(Op::In, 99, 99, true);
        clause_test_case(Op::In, 99, vec![99, 98, 97, 96], true);
        clause_test_case(Op::In, 99.0001, 99.0001, true);
        clause_test_case(Op::In, 99.0001, vec![99.0001, 98.0, 97.0, 96.0], true);
        clause_test_case(Op::LessThan, 1, 1.99999, true);
        clause_test_case(Op::LessThan, 1.99999, 1, false);
        clause_test_case(Op::LessThan, 1, 2, true);
        clause_test_case(Op::LessThanOrEqual, 1, 1.0, true);
        clause_test_case(Op::GreaterThan, 2, 1.99999, true);
        clause_test_case(Op::GreaterThan, 1.99999, 2, false);
        clause_test_case(Op::GreaterThan, 2, 1, true);
        clause_test_case(Op::GreaterThanOrEqual, 1, 1.0, true);
    }

    #[test]
    fn test_string_clauses() {
        clause_test_case(Op::In, "x", "x", true);
        clause_test_case(Op::In, "x", vec!["x", "a", "b", "c"], true);
        clause_test_case(Op::In, "x", "xyz", false);
        clause_test_case(Op::StartsWith, "xyz", "x", true);
        clause_test_case(Op::StartsWith, "x", "xyz", false);
        clause_test_case(Op::EndsWith, "xyz", "z", true);
        clause_test_case(Op::EndsWith, "z", "xyz", false);
        clause_test_case(Op::Contains, "xyz", "y", true);
        clause_test_case(Op::Contains, "y", "xyz", false);
    }

    #[test]
    fn test_mixed_string_and_numbers() {
        clause_test_case(Op::In, "99", 99, false);
        clause_test_case(Op::In, 99, "99", false);
        clause_test_case(Op::Contains, "99", 99, false);
        clause_test_case(Op::StartsWith, "99", 99, false);
        clause_test_case(Op::EndsWith, "99", 99, false);
        clause_test_case(Op::LessThanOrEqual, "99", 99, false);
        clause_test_case(Op::LessThanOrEqual, 99, "99", false);
        clause_test_case(Op::GreaterThanOrEqual, "99", 99, false);
        clause_test_case(Op::GreaterThanOrEqual, 99, "99", false);
    }

    #[test]
    fn test_boolean_equality() {
        clause_test_case(Op::In, true, true, true);
        clause_test_case(Op::In, false, false, true);
        clause_test_case(Op::In, true, false, false);
        clause_test_case(Op::In, false, true, false);
        clause_test_case(Op::In, true, vec![false, true], true);
    }

    #[test]
    fn test_array_equality() {
        // note that the user value must be an array *of arrays*, because a single-level
        // array is interpreted as "any of these values"
        clause_test_case(Op::In, vec![vec!["x"]], vec![vec!["x"]], true);
        clause_test_case(Op::In, vec![vec!["x"]], vec!["x"], false);
        clause_test_case(
            Op::In,
            vec![vec!["x"]],
            vec![vec!["x"], vec!["a"], vec!["b"]],
            true,
        );
    }

    #[test]
    fn test_object_equality() {
        clause_test_case(Op::In, hashmap! {"x" => "1"}, hashmap! {"x" => "1"}, true);
        clause_test_case(
            Op::In,
            hashmap! {"x" => "1"},
            vec![
                hashmap! {"x" => "1"},
                hashmap! {"a" => "2"},
                hashmap! {"b" => "3"},
            ],
            true,
        );
    }

    #[test]
    fn test_regex_match() {
        clause_test_case(Op::Matches, "hello world", "hello.*rld", true);
        clause_test_case(Op::Matches, "hello world", "hello.*orl", true);
        clause_test_case(Op::Matches, "hello world", "l+", true);
        clause_test_case(Op::Matches, "hello world", "(world|planet)", true);
        clause_test_case(Op::Matches, "hello world", "aloha", false);
        clause_test_case(Op::Matches, "hello world", "***bad regex", false);
    }

    #[test]
    fn test_date_clauses() {
        const DATE_STR1: &str = "2017-12-06T00:00:00.000-07:00";
        const DATE_STR2: &str = "2017-12-06T00:01:01.000-07:00";
        const DATE_MS1: i64 = 10000000;
        const DATE_MS2: i64 = 10000001;
        const INVALID_DATE: &str = "hey what's this?";

        clause_test_case(Op::Before, DATE_STR1, DATE_STR2, true);
        clause_test_case(Op::Before, DATE_MS1, DATE_MS2, true);
        clause_test_case(Op::Before, DATE_STR2, DATE_STR1, false);
        clause_test_case(Op::Before, DATE_MS2, DATE_MS1, false);
        clause_test_case(Op::Before, DATE_STR1, DATE_STR1, false);
        clause_test_case(Op::Before, DATE_MS1, DATE_MS1, false);
        clause_test_case(Op::Before, AttributeValue::Null, DATE_STR1, false);
        clause_test_case(Op::Before, DATE_STR1, INVALID_DATE, false);
        clause_test_case(Op::After, DATE_STR2, DATE_STR1, true);
        clause_test_case(Op::After, DATE_MS2, DATE_MS1, true);
        clause_test_case(Op::After, DATE_STR1, DATE_STR2, false);
        clause_test_case(Op::After, DATE_MS1, DATE_MS2, false);
        clause_test_case(Op::After, DATE_STR1, DATE_STR1, false);
        clause_test_case(Op::After, DATE_MS1, DATE_MS1, false);
        clause_test_case(Op::After, AttributeValue::Null, DATE_STR1, false);
        clause_test_case(Op::After, DATE_STR1, INVALID_DATE, false);
    }

    #[test]
    fn test_semver_clauses() {
        clause_test_case(Op::SemVerEqual, "2.0.0", "2.0.0", true);
        clause_test_case(Op::SemVerEqual, "2.0", "2.0.0", true);
        clause_test_case(Op::SemVerEqual, "2-rc1", "2.0.0-rc1", true);
        clause_test_case(Op::SemVerEqual, "2+build2", "2.0.0+build2", true);
        clause_test_case(Op::SemVerEqual, "2.0.0", "2.0.1", false);
        clause_test_case(Op::SemVerLessThan, "2.0.0", "2.0.1", true);
        clause_test_case(Op::SemVerLessThan, "2.0", "2.0.1", true);
        clause_test_case(Op::SemVerLessThan, "2.0.1", "2.0.0", false);
        clause_test_case(Op::SemVerLessThan, "2.0.1", "2.0", false);
        clause_test_case(Op::SemVerLessThan, "2.0.1", "xbad%ver", false);
        clause_test_case(Op::SemVerLessThan, "2.0.0-rc", "2.0.0-rc.beta", true);
        clause_test_case(Op::SemVerGreaterThan, "2.0.1", "2.0", true);
        clause_test_case(Op::SemVerGreaterThan, "10.0.1", "2.0", true);
        clause_test_case(Op::SemVerGreaterThan, "2.0.0", "2.0.1", false);
        clause_test_case(Op::SemVerGreaterThan, "2.0", "2.0.1", false);
        clause_test_case(Op::SemVerGreaterThan, "2.0.1", "xbad%ver", false);
        clause_test_case(Op::SemVerGreaterThan, "2.0.0-rc.1", "2.0.0-rc.0", true);
    }
}
