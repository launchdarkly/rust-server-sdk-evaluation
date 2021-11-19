use serde::Deserialize;

use crate::rule::Clause;
use crate::user::User;
use crate::variation::VariationWeight;
use crate::BucketPrefix;

#[derive(Clone, Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Segment {
    pub key: String,

    pub included: Vec<String>,
    pub excluded: Vec<String>,
    rules: Vec<SegmentRule>,
    salt: String,

    #[serde(default)]
    pub unbounded: bool,
    #[serde(default)]
    generation: Option<i64>,

    pub version: i64,
    #[serde(default)]
    deleted: bool,
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
struct SegmentRule {
    clauses: Vec<Clause>,
    weight: Option<VariationWeight>,
    bucket_by: Option<String>,
}

impl Segment {
    pub fn is_newer_than(&self, segment: &Segment) -> bool {
        self.version > segment.version
    }

    // TODO(ch108586) segment explanations
    pub fn contains(&self, user: &User) -> bool {
        let user_key = user.key().to_string();

        if self.included.contains(&user_key) {
            return true;
        }
        if self.excluded.contains(&user_key) {
            return false;
        }

        for rule in &self.rules {
            if rule.matches(user, &self.key, &self.salt) {
                return true;
            }
        }

        false
    }

    pub fn unbounded_segment_id(&self) -> String {
        match self.generation {
            None | Some(0) => self.key.clone(),
            Some(generation) => format!("{}.g{}", self.key, generation),
        }
    }
}

impl SegmentRule {
    pub fn matches(&self, user: &User, key: &str, salt: &str) -> bool {
        // rules match if _all_ of their clauses do
        for clause in &self.clauses {
            if !clause.matches_non_segment(user) {
                return false;
            }
        }

        match self.weight {
            Some(weight) if weight >= 0.0 => {
                let bucket_by = self.bucket_by.as_deref();
                let prefix = BucketPrefix::KeyAndSalt(key, salt);
                let bucket = user.bucket(bucket_by, prefix);
                bucket < weight / 100_000.0
            }
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::evaluate;
    use crate::flag::Flag;
    use crate::flag_value::FlagValue;
    use crate::store::Store;
    use crate::user::AttributeValue;

    // Treat a Segment as a Store containing only itself
    type TestStore = Segment;
    impl Store for TestStore {
        fn flag(&self, _flag_key: &str) -> Option<&Flag> {
            None
        }
        fn segment(&self, segment_key: &str) -> Option<&Segment> {
            if self.key == segment_key {
                Some(self as &Segment)
            } else {
                None
            }
        }
    }

    fn assert_segment_match(segment: &Segment, user: User, expected: bool) {
        let store = segment as &TestStore;
        let flag = Flag::new_boolean_flag_with_segment_match(vec![&segment.key]);
        let result = evaluate(store, &flag, &user, None);
        assert_eq!(result.value, Some(&FlagValue::Bool(expected)));
    }

    fn new_segment() -> Segment {
        Segment {
            key: "segkey".to_string(),
            included: vec![],
            excluded: vec![],
            rules: vec![],
            salt: "salty".to_string(),
            unbounded: false,
            generation: Some(1),
            version: 1,
            deleted: false,
        }
    }

    fn jane_rule(weight: Option<f32>, bucket_by: Option<String>) -> SegmentRule {
        SegmentRule {
            clauses: vec![Clause::new_match(
                "name",
                AttributeValue::String("Jane".to_string()),
            )],
            weight,
            bucket_by,
        }
    }

    fn thirty_percent_rule(bucket_by: Option<String>) -> SegmentRule {
        SegmentRule {
            clauses: vec![Clause::new_match(
                "key",
                AttributeValue::String(".".to_string()),
            )],
            weight: Some(30_000.0),
            bucket_by,
        }
    }

    #[test]
    fn segment_rule_parse() {
        let rule: SegmentRule =
            serde_json::from_str(r#"{"clauses": [], "weight": null, "bucketBy": null}"#)
                .expect("should parse");
        assert_eq!(
            rule,
            SegmentRule {
                clauses: vec![],
                weight: None,
                bucket_by: None,
            }
        );

        let rule: SegmentRule = serde_json::from_str(
            r#"{
                "clauses":[{
                    "attribute": "name",
                    "negate": false,
                    "op": "matches",
                    "values": ["xyz"]
                }],
                "weight": 10000,
                "bucketBy": "country"
            }"#,
        )
        .expect("should parse");
        assert_eq!(
            rule,
            SegmentRule {
                clauses: vec![Clause::new_match("name", "xyz".into())],
                weight: Some(10_000.0),
                bucket_by: Some("country".to_string()),
            }
        );
    }

    #[test]
    fn segment_match_clause_falls_through_if_segment_not_found() {
        let mut segment = new_segment();
        segment.included.push("foo".to_string());
        segment.key = "different-key".to_string();
        let user = User::with_key("foo").build();
        assert_segment_match(&segment, user, true);
    }

    #[test]
    fn can_match_just_one_segment_from_list() {
        let mut segment = new_segment();
        segment.included.push("foo".to_string());
        let user = User::with_key("foo").build();
        let flag = Flag::new_boolean_flag_with_segment_match(vec![
            "different-segkey",
            "segkey",
            "another-segkey",
        ]);
        let result = evaluate(&segment, &flag, &user, None);
        assert_eq!(result.value, Some(&FlagValue::Bool(true)));
    }

    #[test]
    fn user_is_explicitly_included_in_segment() {
        let mut segment = new_segment();
        segment.included.push("foo".to_string());
        segment.included.push("bar".to_string());
        let user = User::with_key("bar").build();
        assert_segment_match(&segment, user, true);
    }

    #[test]
    fn user_is_matched_by_segment_rule() {
        let mut segment = new_segment();
        segment.rules.push(jane_rule(None, None));
        let jane = User::with_key("foo").name("Jane").build();
        let joan = User::with_key("foo").name("Joan").build();
        assert_segment_match(&segment, jane, true);
        assert_segment_match(&segment, joan, false);
    }

    #[test]
    fn user_is_explicitly_excluded_from_segment() {
        let mut segment = new_segment();
        segment.rules.push(jane_rule(None, None));
        segment.excluded.push("foo".to_string());
        segment.excluded.push("bar".to_string());
        let jane = User::with_key("foo").name("Jane").build();
        assert_segment_match(&segment, jane, false);
    }

    #[test]
    fn segment_includes_override_excludes() {
        let mut segment = new_segment();
        segment.included.push("bar".to_string());
        segment.excluded.push("foo".to_string());
        segment.excluded.push("bar".to_string());
        let user = User::with_key("bar").build();
        assert_segment_match(&segment, user, true);
    }

    #[test]
    fn segment_does_not_match_if_no_includes_or_rules_match() {
        let mut segment = new_segment();
        segment.rules.push(jane_rule(None, None));
        segment.included.push("key".to_string());
        let user = User::with_key("other-key").name("Bob").build();
        assert_segment_match(&segment, user, false);
    }

    #[test]
    fn segment_rule_can_match_user_with_percentage_rollout() {
        let mut segment = new_segment();
        segment.rules.push(jane_rule(Some(99_999.0), None));
        let user = User::with_key("key").name("Jane").build();
        assert_segment_match(&segment, user, true);
    }

    #[test]
    fn segment_rule_can_not_match_user_with_percentage_rollout() {
        let mut segment = new_segment();
        segment.rules.push(jane_rule(Some(1.0), None));
        let user = User::with_key("key").name("Jane").build();
        assert_segment_match(&segment, user, false);
    }

    #[test]
    fn segment_rule_can_have_percentage_rollout() {
        let mut segment = new_segment();
        segment.rules.push(thirty_percent_rule(None));

        let user_a = User::with_key("userKeyA").build(); // bucket 0.14574753
        let user_z = User::with_key("userKeyZ").build(); // bucket 0.45679215
        assert_segment_match(&segment, user_a, true);
        assert_segment_match(&segment, user_z, false);
    }

    #[test]
    fn segment_rule_can_have_percentage_rollout_by_any_attribute() {
        let mut segment = new_segment();
        segment
            .rules
            .push(thirty_percent_rule(Some("name".to_string())));
        let user_a = User::with_key("x").name("userKeyA").build(); // bucket 0.14574753
        let user_z = User::with_key("x").name("userKeyZ").build(); // bucket 0.45679215
        assert_segment_match(&segment, user_a, true);
        assert_segment_match(&segment, user_z, false);
    }

    #[test]
    fn test_segment_can_determine_which_is_newer() {
        let oldest = Segment {
            key: "oldest".into(),
            included: Vec::new(),
            excluded: Vec::new(),
            rules: Vec::new(),
            salt: "salty".into(),
            unbounded: false,
            generation: None,
            version: 1,
            deleted: false,
        };
        let mut middle = oldest.clone();
        middle.version = 2;

        let mut newest = middle.clone();
        newest.version = 3;

        assert!(newest.is_newer_than(&middle));
        assert!(newest.is_newer_than(&oldest));
        assert!(middle.is_newer_than(&oldest));

        assert!(!oldest.is_newer_than(&middle));
        assert!(!oldest.is_newer_than(&newest));
    }
}
