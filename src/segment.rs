use serde::Deserialize;

use crate::rule::Clause;
use crate::user::User;

#[derive(Clone, Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Segment {
    pub key: String,

    included: Vec<String>,
    excluded: Vec<String>,
    rules: Vec<SegmentRule>,
    salt: String,
}

#[derive(Clone, Debug, Deserialize)]
struct SegmentRule {
    clauses: Vec<Clause>,
    // TODO rollout
    // weight: Option<VariationWeight>
    // bucket_by: Option<String>,
}

impl Segment {
    // TODO segment explanations
    pub fn contains(&self, user: &User) -> bool {
        let user_key = user.key();

        if self.included.contains(user_key) {
            return true;
        }
        if self.excluded.contains(user_key) {
            return false;
        }

        for rule in &self.rules {
            if rule.matches(user) {
                return true;
            }
        }

        false
    }
}

impl SegmentRule {
    pub fn matches(&self, user: &User) -> bool {
        // rules match if _all_ of their clauses do
        for clause in &self.clauses {
            if !clause.matches_non_segment(user) {
                return false;
            }
        }
        true
    }
}
