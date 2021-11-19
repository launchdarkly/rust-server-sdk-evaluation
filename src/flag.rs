use serde::Deserialize;

use crate::eval::{self, Detail, Reason};
use crate::flag_value::FlagValue;
use crate::rule::FlagRule;
use crate::user::User;
use crate::variation::{VariationIndex, VariationOrRollout};
use crate::BucketResult;

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Flag {
    pub key: String,
    #[serde(default)]
    pub version: u64,
    #[serde(default)]
    deleted: bool,

    pub(crate) on: bool,

    pub(crate) targets: Vec<Target>,
    pub(crate) rules: Vec<FlagRule>,
    pub(crate) prerequisites: Vec<Prereq>,

    pub(crate) fallthrough: VariationOrRollout,
    pub(crate) off_variation: Option<VariationIndex>,
    variations: Vec<FlagValue>,
    pub client_side_availability: ClientSideAvailability,

    salt: String,

    #[serde(default)]
    pub track_events: bool,
    #[serde(default)]
    pub track_events_fallthrough: bool,
    #[serde(default)]
    pub debug_events_until_date: Option<u64>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Prereq {
    pub(crate) key: String,
    pub(crate) variation: VariationIndex,
}

#[derive(Clone, Debug, Deserialize)]
pub(crate) struct Target {
    pub(crate) values: Vec<String>,
    pub(crate) variation: VariationIndex,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ClientSideAvailability {
    pub using_mobile_key: bool,
    pub using_environment_id: bool,
}

impl Flag {
    pub fn is_newer_than(&self, flag: &Flag) -> bool {
        self.version > flag.version
    }

    pub fn variation(&self, index: VariationIndex, reason: Reason) -> Detail<&FlagValue> {
        Detail {
            value: self.variations.get(index),
            variation_index: Some(index),
            reason,
        }
        .should_have_value(eval::Error::MalformedFlag)
    }

    pub fn off_value(&self, reason: Reason) -> Detail<&FlagValue> {
        match self.off_variation {
            Some(index) => self.variation(index, reason),
            None => Detail::empty(reason),
        }
    }

    pub(crate) fn resolve_variation_or_rollout(
        &self,
        vr: &VariationOrRollout,
        user: &User,
    ) -> Result<BucketResult, eval::Error> {
        vr.variation(&self.key, user, &self.salt)
            .ok_or(eval::Error::MalformedFlag)
    }

    pub fn is_experimentation_enabled(&self, reason: &Reason) -> bool {
        match reason {
            _ if reason.is_in_experiment() => true,
            Reason::Fallthrough { .. } => self.track_events_fallthrough,
            Reason::RuleMatch { rule_index, .. } => self
                .rules
                .get(*rule_index)
                .map(|rule| rule.track_events)
                .unwrap_or(false),
            _ => false,
        }
    }

    #[cfg(test)]
    pub fn new_boolean_flag_with_segment_match(segment_keys: Vec<&str>) -> Self {
        Self {
            key: "feature".to_string(),
            version: 1,
            deleted: false,
            on: true,
            targets: vec![],
            rules: vec![crate::rule::FlagRule::new_segment_match(segment_keys)],
            prerequisites: vec![],
            fallthrough: VariationOrRollout::Variation { variation: 0 },
            off_variation: Some(0),
            variations: vec![FlagValue::Bool(false), FlagValue::Bool(true)],
            client_side_availability: ClientSideAvailability {
                using_mobile_key: false,
                using_environment_id: false,
            },
            salt: "xyz".to_string(),
            track_events: false,
            track_events_fallthrough: false,
            debug_events_until_date: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Flag;
    use crate::store::Store;
    use crate::test_common::TestStore;
    use crate::{ClientSideAvailability, VariationOrRollout};
    use spectral::prelude::*;

    use crate::eval::Reason::*;

    #[test]
    fn is_experimentation_enabled() {
        let store = TestStore::new();

        let flag = store.flag("flag").unwrap();
        asserting!("defaults to false")
            .that(&flag.is_experimentation_enabled(&Off))
            .is_false();
        asserting!("false for fallthrough if trackEventsFallthrough is false")
            .that(&flag.is_experimentation_enabled(&Fallthrough {
                in_experiment: false,
            }))
            .is_false();

        let flag = store.flag("flagWithRuleExclusion").unwrap();
        asserting!("true for fallthrough if trackEventsFallthrough is true")
            .that(&flag.is_experimentation_enabled(&Fallthrough {
                in_experiment: false,
            }))
            .is_true();
        asserting!("true for rule if rule.trackEvents is true")
            .that(&flag.is_experimentation_enabled(&RuleMatch {
                rule_index: 0,
                rule_id: flag.rules.get(0).unwrap().id.clone(),
                in_experiment: false,
            }))
            .is_true();

        let flag = store.flag("flagWithExperiment").unwrap();
        asserting!("true for fallthrough if reason says it is")
            .that(&flag.is_experimentation_enabled(&Fallthrough {
                in_experiment: true,
            }))
            .is_true();
        asserting!("false for fallthrough if reason says it is")
            .that(&flag.is_experimentation_enabled(&Fallthrough {
                in_experiment: false,
            }))
            .is_false();
        // note this flag doesn't even have a rule - doesn't matter, we go by the reason
        asserting!("true for rule if reason says it is")
            .that(&flag.is_experimentation_enabled(&RuleMatch {
                rule_index: 42,
                rule_id: "lol".into(),
                in_experiment: true,
            }))
            .is_true();
        asserting!("false for rule if reason says it is")
            .that(&flag.is_experimentation_enabled(&RuleMatch {
                rule_index: 42,
                rule_id: "lol".into(),
                in_experiment: false,
            }))
            .is_false();
    }

    #[test]
    fn test_flag_can_determine_which_is_newer() {
        let oldest = Flag {
            key: "oldest".into(),
            version: 1,
            deleted: false,
            on: true,
            targets: Vec::new(),
            rules: Vec::new(),
            prerequisites: Vec::new(),
            fallthrough: VariationOrRollout::Variation { variation: 1 },
            off_variation: None,
            variations: Vec::new(),
            client_side_availability: ClientSideAvailability {
                using_mobile_key: false,
                using_environment_id: false,
            },
            salt: "salty".into(),
            track_events: false,
            track_events_fallthrough: false,
            debug_events_until_date: None,
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
