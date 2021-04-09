use serde::Deserialize;

use crate::eval::{self, Detail, Reason};
use crate::flag_value::FlagValue;
use crate::rule::FlagRule;
use crate::store::Store;
use crate::user::User;
use crate::variation::{VariationIndex, VariationOrRollout, VariationOrRolloutOrMalformed};

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Flag {
    pub key: String,
    pub version: u64,
    #[serde(default)]
    deleted: bool,

    on: bool,

    targets: Vec<Target>,
    rules: Vec<FlagRule>,
    prerequisites: Vec<Prereq>,

    fallthrough: VariationOrRolloutOrMalformed,
    off_variation: Option<VariationIndex>,
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
    key: String,
    variation: VariationIndex,
}

#[derive(Clone, Debug, Deserialize)]
struct Target {
    values: Vec<String>,
    variation: VariationIndex,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ClientSideAvailability {
    pub using_mobile_key: bool,
    pub using_environment_id: bool,
}

impl Flag {
    pub fn evaluate(&self, user: &User, store: &dyn Store) -> Detail<&FlagValue> {
        if !self.on {
            return self.off_value(Reason::Off);
        }

        for prereq in &self.prerequisites {
            if let Some(flag) = store.flag(&prereq.key) {
                if !flag.on || flag.evaluate(user, store).variation_index != Some(prereq.variation)
                {
                    // TODO capture prereq event
                    return self.off_value(Reason::PrerequisiteFailed {
                        prerequisite_key: prereq.key.to_string(),
                    });
                }
            } else {
                return self.off_value(Reason::PrerequisiteFailed {
                    prerequisite_key: prereq.key.to_string(),
                });
            }
        }

        for target in &self.targets {
            for value in &target.values {
                if value == user.key() {
                    return self.variation(target.variation, Reason::TargetMatch);
                }
            }
        }

        for (rule_index, rule) in self.rules.iter().enumerate() {
            if rule.matches(&user, store) {
                return self.value_for_variation_or_rollout(
                    &rule.variation_or_rollout,
                    &user,
                    Reason::RuleMatch {
                        rule_index,
                        rule_id: rule.id.clone(),
                    },
                );
            }
        }

        // just return the fallthrough for now
        self.fallthrough
            .get()
            .as_ref()
            .ok()
            .map(|vor| self.value_for_variation_or_rollout(vor, &user, Reason::Fallthrough))
            .unwrap_or_else(|| Detail::err(eval::Error::MalformedFlag))
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

    fn value_for_variation_or_rollout(
        &self,
        vr: &VariationOrRollout,
        user: &User,
        reason: Reason,
    ) -> Detail<&FlagValue> {
        vr.variation(&self.key, user, &self.salt)
            .map_or(Detail::err(eval::Error::MalformedFlag), |variation| {
                self.variation(variation, reason)
            })
    }

    pub fn is_experimentation_enabled(&self, reason: &Reason) -> bool {
        match reason {
            Reason::Fallthrough => self.track_events_fallthrough,
            Reason::RuleMatch {
                rule_index,
                rule_id: _,
            } => self
                .rules
                .get(*rule_index)
                .map(|rule| rule.track_events)
                .unwrap_or(false),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use maplit::hashmap;
    use spectral::prelude::*;

    use crate::eval::Reason::*;
    use crate::flag_value::FlagValue::Bool;
    use crate::segment::Segment;

    struct TestStore {
        flags: HashMap<String, Flag>,
        segments: HashMap<String, Segment>,
    }

    impl TestStore {
        fn new() -> TestStore {
            TestStore {
                flags: hashmap! {
                    "flag".to_string() => serde_json::from_str(r#"{
                        "key": "flag",
                        "version": 42,
                        "on": false,
                        "targets": [],
                        "rules": [],
                        "prerequisites": [],
                        "fallthrough": {"variation": 1},
                        "offVariation": 0,
                        "variations": [false, true],
                        "clientSideAvailability": {
                            "usingEnvironmentId": true,
                            "usingMobileKey": true
                        },
                        "salt": "salty"
                    }"#).unwrap(),
                    "flagWithTrackAndDebugEvents".to_string() => serde_json::from_str(r#"{
                        "key": "flag",
                        "version": 42,
                        "on": false,
                        "targets": [],
                        "rules": [],
                        "prerequisites": [],
                        "fallthrough": {"variation": 1},
                        "offVariation": 0,
                        "variations": [false, true],
                        "clientSideAvailability": {
                            "usingEnvironmentId": true,
                            "usingMobileKey": true
                        },
                        "salt": "salty",
                        "trackEvents": true,
                        "trackEventsFallthrough": true,
                        "debugEventsUntilDate": 1500000000
                    }"#).unwrap(),
                    "flagWithTarget".to_string() => serde_json::from_str(r#"{
                        "key": "flagWithTarget",
                        "version": 42,
                        "on": false,
                        "targets": [{
                            "values": ["bob"],
                            "variation": 0
                        }],
                        "rules": [],
                        "prerequisites": [],
                        "fallthrough": {"variation": 1},
                        "offVariation": 0,
                        "variations": [false, true],
                        "clientSideAvailability": {
                            "usingEnvironmentId": true,
                            "usingMobileKey": true
                        },
                        "salt": "salty"
                    }"#).unwrap(),
                    "flagWithMissingPrereq".to_string() => serde_json::from_str(r#"{
                        "key": "flagWithMissingPrereq",
                        "version": 42,
                        "on": true,
                        "targets": [],
                        "rules": [],
                        "prerequisites": [{
                            "key": "badPrereq",
                            "variation": 1
                        }],
                        "fallthrough": {"variation": 1},
                        "offVariation": 0,
                        "variations": [false, true],
                        "clientSideAvailability": {
                            "usingEnvironmentId": true,
                            "usingMobileKey": true
                        },
                        "salt": "salty"
                    }"#).unwrap(),
                    "flagWithOffPrereq".to_string() => serde_json::from_str(r#"{
                        "key": "flagWithOffPrereq",
                        "version": 42,
                        "on": true,
                        "targets": [],
                        "rules": [],
                        "prerequisites": [{
                            "key": "offPrereq",
                            "variation": 1
                        }],
                        "fallthrough": {"variation": 1},
                        "offVariation": 0,
                        "variations": [false, true],
                        "clientSideAvailability": {
                            "usingEnvironmentId": true,
                            "usingMobileKey": true
                        },
                        "salt": "salty"
                    }"#).unwrap(),
                    "flagWithSatisfiedPrereq".to_string() => serde_json::from_str(r#"{
                        "key": "flagWithSatisfiedPrereq",
                        "version": 42,
                        "on": true,
                        "targets": [],
                        "rules": [],
                        "prerequisites": [{
                            "key": "prereq",
                            "variation": 1
                        }],
                        "fallthrough": {"variation": 1},
                        "offVariation": 0,
                        "variations": [false, true],
                        "clientSideAvailability": {
                            "usingEnvironmentId": true,
                            "usingMobileKey": true
                        },
                        "salt": "salty"
                    }"#).unwrap(),
                    "prereq".to_string() => serde_json::from_str(r#"{
                        "key": "prereq",
                        "version": 42,
                        "on": true,
                        "targets": [{
                            "values": ["bob"],
                            "variation": 0
                        }],
                        "rules": [],
                        "prerequisites": [],
                        "fallthrough": {"variation": 1},
                        "offVariation": 0,
                        "variations": [false, true],
                        "clientSideAvailability": {
                            "usingEnvironmentId": true,
                            "usingMobileKey": true
                        },
                        "salt": "salty"
                    }"#).unwrap(),
                    "offPrereq".to_string() => serde_json::from_str(r#"{
                        "key": "offPrereq",
                        "version": 42,
                        "on": false,
                        "targets": [],
                        "rules": [],
                        "prerequisites": [],
                        "fallthrough": {"variation": 1},
                        "offVariation": 1,
                        "variations": [false, true],
                        "clientSideAvailability": {
                            "usingEnvironmentId": true,
                            "usingMobileKey": true
                        },
                        "salt": "salty"
                    }"#).unwrap(),
                    "flagWithInRule".to_string() => serde_json::from_str(r#"{
                        "key": "flagWithInRule",
                        "version": 42,
                        "on": false,
                        "targets": [],
                        "rules": [{
                            "id": "in-rule",
                            "clauses": [{
                                "attribute": "team",
                                "negate": false,
                                "op": "in",
                                "values": ["Avengers"]
                            }],
                            "variation": 0,
                            "trackEvents": false
                        }],
                        "prerequisites": [],
                        "fallthrough": {"variation": 1},
                        "offVariation": 0,
                        "variations": [false, true],
                        "clientSideAvailability": {
                            "usingEnvironmentId": true,
                            "usingMobileKey": true
                        },
                        "salt": "salty"
                    }"#).unwrap(),
                    "flagWithSegmentMatchRule".to_string() => serde_json::from_str(r#"{
                        "key": "flagWithSegmentMatchRule",
                        "version": 42,
                        "on": true,
                        "targets": [],
                        "rules": [{
                            "id": "match-rule",
                            "clauses": [{
                                "attribute": "segmentMatch",
                                "negate": false,
                                "op": "segmentMatch",
                                "values": ["segment"]
                            }],
                            "variation": 0,
                            "trackEvents": false
                        }],
                        "prerequisites": [],
                        "fallthrough": {"variation": 1},
                        "offVariation": 0,
                        "variations": [false, true],
                        "clientSideAvailability": {
                            "usingEnvironmentId": true,
                            "usingMobileKey": true
                        },
                        "salt": "salty"
                    }"#).unwrap(),
                },
                segments: hashmap! {
                    "segment".to_string() => serde_json::from_str(r#"{
                        "key": "segment",
                        "included": ["alice"],
                        "excluded": [],
                        "rules": [],
                        "salt": "salty",
                        "version": 1
                    }"#).unwrap()
                },
            }
        }

        fn update_flag(&mut self, flag_key: &str, fun: fn(&mut Flag) -> ()) {
            let flag = self.flags.get_mut(flag_key).unwrap();
            fun(flag);
        }
    }

    impl Store for TestStore {
        fn flag(&self, flag_key: &str) -> Option<&Flag> {
            self.flags.get(flag_key)
        }

        fn segment(&self, segment_key: &str) -> Option<&Segment> {
            self.segments.get(segment_key)
        }
    }

    #[test]
    fn test_eval_flag_basic() {
        let store = TestStore::new();
        let alice = User::with_key("alice").build(); // not targeted
        let bob = User::with_key("bob").build(); // targeted
        let mut flag = store.flag("flagWithTarget").unwrap().clone();

        assert!(!flag.on);
        let detail = flag.evaluate(&alice, &store);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.variation_index).contains_value(0);
        assert_that!(detail.reason).is_equal_to(&Off);

        assert_that!(flag.evaluate(&bob, &store)).is_equal_to(&detail);

        // flip off variation
        flag.off_variation = Some(1);
        let detail = flag.evaluate(&alice, &store);
        assert_that!(detail.value).contains_value(&Bool(true));
        assert_that!(detail.variation_index).contains_value(1);

        // off variation unspecified
        flag.off_variation = None;
        let detail = flag.evaluate(&alice, &store);
        assert_that!(detail.value).is_none();
        assert_that!(detail.variation_index).is_none();
        assert_that!(detail.reason).is_equal_to(&Off);

        // flip targeting on
        flag.on = true;
        let detail = flag.evaluate(&alice, &store);
        assert_that!(detail.value).contains_value(&Bool(true));
        assert_that!(detail.variation_index).contains_value(1);
        assert_that!(detail.reason).is_equal_to(&Fallthrough);

        let detail = flag.evaluate(&bob, &store);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.variation_index).contains_value(0);
        assert_that!(detail.reason).is_equal_to(&TargetMatch);

        // flip default variation
        flag.fallthrough = VariationOrRollout::Variation(0).into();
        let detail = flag.evaluate(&alice, &store);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.variation_index).contains_value(0);

        // bob's reason should still be TargetMatch even though his value is now the default
        let detail = flag.evaluate(&bob, &store);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.variation_index).contains_value(0);
        assert_that!(detail.reason).is_equal_to(&TargetMatch);
    }

    #[test]
    fn test_eval_flag_rules() {
        let store = TestStore::new();
        let alice = User::with_key("alice").build();
        let bob = User::with_key("bob")
            .custom(hashmap! {
                "team".into() => "Avengers".into(),
            })
            .build();

        let mut flag = store.flag("flagWithInRule").unwrap().clone();

        assert!(!flag.on);
        for user in vec![&alice, &bob] {
            let detail = flag.evaluate(user, &store);
            assert_that!(detail.value).contains_value(&Bool(false));
            assert_that!(detail.variation_index).contains_value(0);
            assert_that!(detail.reason).is_equal_to(&Off);
        }

        // flip targeting on
        flag.on = true;
        let detail = flag.evaluate(&alice, &store);
        assert_that!(detail.value).contains_value(&Bool(true));
        assert_that!(detail.variation_index).contains_value(1);
        assert_that!(detail.reason).is_equal_to(&Fallthrough);

        let detail = flag.evaluate(&bob, &store);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.variation_index).contains_value(0);
        assert_that!(detail.reason).is_equal_to(&RuleMatch {
            rule_id: "in-rule".to_string(),
            rule_index: 0,
        });
    }

    #[test]
    fn test_eval_flag_unsatisfied_prereq() {
        let store = TestStore::new();
        let flag = store.flag("flagWithMissingPrereq").unwrap().clone();
        assert!(flag.on);

        let alice = User::with_key("alice").build();
        let bob = User::with_key("bob").build();

        for user in vec![&alice, &bob] {
            let detail = flag.evaluate(user, &store);
            assert_that!(detail.value).contains_value(&Bool(false));
            assert_that!(detail.reason).is_equal_to(&PrerequisiteFailed {
                prerequisite_key: "badPrereq".to_string(),
            });
        }
    }

    #[test]
    fn test_eval_flag_off_prereq() {
        let store = TestStore::new();
        let flag = store.flag("flagWithOffPrereq").unwrap().clone();
        assert!(flag.on);

        let alice = User::with_key("alice").build();

        let detail = flag.evaluate(&alice, &store);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.reason).is_equal_to(&PrerequisiteFailed {
            prerequisite_key: "offPrereq".to_string(),
        });
    }

    #[test]
    fn test_eval_flag_satisfied_prereq() {
        let mut store = TestStore::new();
        let flag = store.flag("flagWithSatisfiedPrereq").unwrap().clone();

        let alice = User::with_key("alice").build();
        let bob = User::with_key("bob").build();

        let detail = flag.evaluate(&alice, &store);
        asserting!("alice should pass prereq and see fallthrough")
            .that(&detail.value)
            .contains_value(&Bool(true));
        let detail = flag.evaluate(&bob, &store);
        asserting!("bob should see prereq failed due to target")
            .that(&detail.value)
            .contains_value(&Bool(false));
        assert_that!(detail.reason).is_equal_to(Reason::PrerequisiteFailed {
            prerequisite_key: "prereq".to_string(),
        });

        // prerequisite off
        store.update_flag("prereq", |flag| (flag.on = false));
        for user in vec![&alice, &bob] {
            let detail = flag.evaluate(user, &store);
            assert_that!(detail.value).contains_value(&Bool(false));
            assert_that!(detail.reason).is_equal_to(&PrerequisiteFailed {
                prerequisite_key: "prereq".to_string(),
            });
        }
    }

    #[test]
    fn test_eval_flag_segments() {
        let store = TestStore::new();
        let flag = store.flag("flagWithSegmentMatchRule").unwrap();

        let alice = User::with_key("alice").build();
        let bob = User::with_key("bob").build();

        let detail = flag.evaluate(&alice, &store);
        asserting!("alice is in segment, should see false with RuleMatch")
            .that(&detail.value)
            .contains_value(&Bool(false));
        assert_that!(detail.reason).is_equal_to(Reason::RuleMatch {
            rule_id: "match-rule".to_string(),
            rule_index: 0,
        });
        let detail = flag.evaluate(&bob, &store);
        asserting!("bob is not in segment and should see fallthrough")
            .that(&detail.value)
            .contains_value(&Bool(true));
        assert_that!(detail.reason).is_equal_to(Reason::Fallthrough);
    }
}
