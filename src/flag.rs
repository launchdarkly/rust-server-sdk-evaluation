use serde::Deserialize;

use crate::eval::{self, Detail, Reason};
use crate::flag_value::FlagValue;
use crate::rule::FlagRule;
use crate::store::Store;
use crate::user::{AttributeValue, User};
use crate::variation::{VariationIndex, VariationOrRollout, VariationOrRolloutOrMalformed};

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Flag {
    pub key: String,
    pub version: u64,

    on: bool,

    targets: Vec<Target>,
    rules: Vec<FlagRule>,
    prerequisites: Vec<Prereq>,

    fallthrough: VariationOrRolloutOrMalformed,
    off_variation: Option<VariationIndex>,
    variations: Vec<FlagValue>,

    salt: String,
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

impl Flag {
    pub fn evaluate(&self, user: &User, store: &dyn Store) -> Detail<&FlagValue> {
        if user.key().is_none() {
            return Detail::err(eval::Error::UserNotSpecified);
        }

        if !self.on {
            return self.off_value(Reason::Off);
        }

        for prereq in &self.prerequisites {
            if let Some(flag) = store.flag(&prereq.key) {
                if flag.evaluate(user, store).variation_index != Some(prereq.variation) {
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
                if Some(value) == user.key() {
                    return self.variation(target.variation, Reason::TargetMatch);
                }
            }
        }

        for rule in &self.rules {
            if rule.matches(&user, store) {
                return self.value_for_variation_or_rollout(
                    &rule.variation_or_rollout,
                    &user,
                    Reason::RuleMatch,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use spectral::prelude::*;
    use maplit::hashmap;
    use crate::rule::{Clause, Op};
    use crate::segment::Segment;
    use crate::flag_value::FlagValue::Bool;
    use crate::eval::Reason::*;
    use std::collections::HashMap;

    struct TestStore {
        flags: HashMap<String, Flag>,
        segments: HashMap<String, Segment>,
    }

    impl TestStore {
        fn new() -> TestStore{
            TestStore{
                flags: hashmap!{
                    "flag".to_string() => Flag {
                        key: "flag".to_string(),
                        version: 42,
                        on: false,
                        targets: vec![],
                        rules: vec![],
                        prerequisites: vec![],
                        fallthrough: VariationOrRolloutOrMalformed::VariationOrRollout(
                            VariationOrRollout::Variation(1),
                        ),
                        off_variation: Some(0),
                        variations: vec![false.into(), true.into()],
                        salt: "salty".to_string(),
                    },
                    "flagWithTarget".to_string() => Flag {
                        key: "flagWithTarget".to_string(),
                        version: 42,
                        on: false,
                        targets: vec![Target {
                            values: vec!["bob".into()],
                            variation: 0,
                        }],
                        rules: vec![],
                        prerequisites: vec![],
                        fallthrough: VariationOrRolloutOrMalformed::VariationOrRollout(
                            VariationOrRollout::Variation(1),
                        ),
                        off_variation: Some(0),
                        variations: vec![false.into(), true.into()],
                        salt: "salty".to_string(),
                    },
                    "flagWithUnsatisfiedPrereq".to_string() => Flag {
                        key: "flagWithUnsatisfiedPrereq".to_string(),
                        version: 42,
                        on: true,
                        targets: vec![],
                        rules: vec![],
                        prerequisites: vec![Prereq {
                            key: "badPrereq".to_string(),
                            variation: 1,
                        }],
                        fallthrough: VariationOrRolloutOrMalformed::VariationOrRollout(
                            VariationOrRollout::Variation(1),
                        ),
                        off_variation: Some(0),
                        variations: vec![false.into(), true.into()],
                        salt: "salty".to_string(),
                    },
                    "flagWithSatisfiedPrereq".to_string() => Flag {
                        key: "flagWithSatisfiedPrereq".to_string(),
                        version: 42,
                        on: true,
                        targets: vec![],
                        rules: vec![],
                        prerequisites: vec![Prereq {
                            key: "prereq".to_string(),
                            variation: 1,
                        }],
                        fallthrough: VariationOrRolloutOrMalformed::VariationOrRollout(
                            VariationOrRollout::Variation(1),
                        ),
                        off_variation: Some(0),
                        variations: vec![false.into(), true.into()],
                        salt: "salty".to_string(),
                    },
                    "prereq".to_string() => Flag {
                        key: "prereq".to_string(),
                        version: 42,
                        on: true,
                        targets: vec![Target {
                            values: vec!["bob".into()],
                            variation: 0,
                        }],
                        rules: vec![],
                        prerequisites: vec![],
                        fallthrough: VariationOrRolloutOrMalformed::VariationOrRollout(
                            VariationOrRollout::Variation(1),
                        ),
                        off_variation: Some(0),
                        variations: vec![false.into(), true.into()],
                        salt: "salty".to_string(),
                    },
                    "flagWithInRule".to_string() => Flag {
                        key: "flagWithInRule".to_string(),
                        version: 42,
                        on: false,
                        targets: vec![],
                        rules: vec![FlagRule {
                            clauses: vec![ Clause {
                                attribute: "team".to_string(),
                                negate: false,
                                op: Op::In,
                                values: vec![AttributeValue::String("Avengers".to_string())],
                            }],
                            variation_or_rollout: VariationOrRollout::Variation(0),
                        }],
                        prerequisites: vec![],
                        fallthrough: VariationOrRolloutOrMalformed::VariationOrRollout(
                            VariationOrRollout::Variation(1),
                        ),
                        off_variation: Some(0),
                        variations: vec![false.into(), true.into()],
                        salt: "salty".to_string(),
                    },
                    "flagWithSegmentMatchRule".to_string() => Flag {
                        key: "flagWithSegmentMatchRule".to_string(),
                        version: 42,
                        on: true,
                        targets: vec![],
                        rules: vec![FlagRule {
                            clauses: vec![ Clause {
                                attribute: "segmentMatch".to_string(),
                                negate: false,
                                op: Op::SegmentMatch,
                                values: vec!["segment".into()],
                            }],
                            variation_or_rollout: VariationOrRollout::Variation(0),
                        }],
                        prerequisites: vec![],
                        fallthrough: VariationOrRolloutOrMalformed::VariationOrRollout(
                            VariationOrRollout::Variation(1),
                        ),
                        off_variation: Some(0),
                        variations: vec![false.into(), true.into()],
                        salt: "salty".to_string(),
                    },
                },
                segments: hashmap!{
                    "segment".to_string() => Segment{
                        key: "segment".to_string(),
                        included: vec!["alice".to_string()],
                        excluded: vec![],
                        rules: vec![],
                        salt: "salty".to_string(),
                    },
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
        let mut store = TestStore::new();
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
        assert_that!(detail.reason).is_equal_to(&RuleMatch);
    }

    #[test]
    fn test_eval_flag_unsatisfied_prereq() {
        let store = TestStore::new();
        let flag = store.flag("flagWithUnsatisfiedPrereq").unwrap().clone();
        assert!(flag.on);

        let alice = User::with_key("alice").build();
        let bob = User::with_key("bob").build();

        // prerequisite missing => prerequisite failed.
        for user in vec![&alice, &bob] {
            let detail = flag.evaluate(user, &store);
            assert_that!(detail.value).contains_value(&Bool(false));
            assert_that!(detail.reason).is_equal_to(&PrerequisiteFailed {
                prerequisite_key: "badPrereq".to_string(),
            });
        }
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
        assert_that!(detail.reason).is_equal_to(Reason::RuleMatch);
        let detail = flag.evaluate(&bob, &store);
        asserting!("bob is not in segment and should see fallthrough")
            .that(&detail.value)
            .contains_value(&Bool(true));
        assert_that!(detail.reason).is_equal_to(Reason::Fallthrough);
    }
}
