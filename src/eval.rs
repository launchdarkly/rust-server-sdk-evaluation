use serde::Serialize;

use crate::flag::Flag;
use crate::flag_value::FlagValue;
use crate::store::Store;
use crate::user::User;
use crate::variation::VariationIndex;
use crate::BucketResult;

pub struct PrerequisiteEvent {
    pub target_flag_key: String,
    pub user: User,
    pub prerequisite_flag: Flag,
    pub prerequisite_result: Detail<FlagValue>,
}

pub trait PrerequisiteEventRecorder {
    fn record(&self, event: PrerequisiteEvent);
}

pub fn evaluate<'a>(
    store: &'a dyn Store,
    flag: &'a Flag,
    user: &'a User,
    prerequisite_event_recorder: Option<&dyn PrerequisiteEventRecorder>,
) -> Detail<&'a FlagValue> {
    if !flag.on {
        return flag.off_value(Reason::Off);
    }

    for prereq in &flag.prerequisites {
        if let Some(prereq_flag) = store.flag(&prereq.key) {
            let prerequisite_result =
                evaluate(store, prereq_flag, user, prerequisite_event_recorder);
            let variation_index = prerequisite_result.variation_index;

            if let Some(recorder) = prerequisite_event_recorder {
                recorder.record(PrerequisiteEvent {
                    target_flag_key: flag.key.clone(),
                    user: user.clone(),
                    prerequisite_flag: prereq_flag.clone(),
                    prerequisite_result: prerequisite_result.map(|v| v.clone()),
                });
            }

            if !prereq_flag.on || variation_index != Some(prereq.variation) {
                return flag.off_value(Reason::PrerequisiteFailed {
                    prerequisite_key: prereq.key.to_string(),
                });
            }
        } else {
            return flag.off_value(Reason::PrerequisiteFailed {
                prerequisite_key: prereq.key.to_string(),
            });
        }
    }

    for target in &flag.targets {
        for value in &target.values {
            if value == user.key() {
                return flag.variation(target.variation, Reason::TargetMatch);
            }
        }
    }

    for (rule_index, rule) in flag.rules.iter().enumerate() {
        if rule.matches(user, &*store) {
            let result = flag.resolve_variation_or_rollout(&rule.variation_or_rollout, user);
            return match result {
                Ok(BucketResult {
                    variation_index,
                    in_experiment,
                }) => {
                    let reason = Reason::RuleMatch {
                        rule_index,
                        rule_id: rule.id.clone(),
                        in_experiment,
                    };
                    flag.variation(variation_index, reason)
                }
                Err(e) => Detail::err(e),
            };
        }
    }

    let result = flag.resolve_variation_or_rollout(&flag.fallthrough, user);
    return match result {
        Ok(BucketResult {
            variation_index,
            in_experiment,
        }) => {
            let reason = Reason::Fallthrough { in_experiment };
            flag.variation(variation_index, reason)
        }
        Err(e) => Detail::err(e),
    };
}

#[derive(Clone, Debug, PartialEq)]
pub struct Detail<T> {
    pub value: Option<T>,
    pub variation_index: Option<VariationIndex>,
    pub reason: Reason,
}

impl<T> Detail<T> {
    pub fn empty(reason: Reason) -> Detail<T> {
        Detail {
            value: None,
            variation_index: None,
            reason,
        }
    }

    pub fn err_default(error: Error, default: T) -> Detail<T> {
        Detail {
            value: Some(default),
            variation_index: None,
            reason: Reason::Error { error },
        }
    }

    pub fn err(error: Error) -> Detail<T> {
        Detail::empty(Reason::Error { error })
    }

    pub fn map<U, F>(self, f: F) -> Detail<U>
    where
        F: FnOnce(T) -> U,
    {
        Detail {
            value: self.value.map(f),
            variation_index: self.variation_index,
            reason: self.reason,
        }
    }

    pub fn should_have_value(mut self, e: Error) -> Detail<T> {
        if self.value.is_none() {
            self.reason = Reason::Error { error: e };
        }
        self
    }

    pub fn try_map<U, F>(self, f: F, e: Error) -> Detail<U>
    where
        F: FnOnce(T) -> Option<U>,
    {
        if self.value.is_none() {
            return Detail {
                value: None,
                variation_index: self.variation_index,
                reason: self.reason,
            };
        }
        match f(self.value.unwrap()) {
            Some(v) => Detail {
                value: Some(v),
                variation_index: self.variation_index,
                reason: self.reason,
            },
            None => Detail::err(e),
        }
    }

    pub fn or(mut self, default: T) -> Detail<T> {
        if self.value.is_none() {
            self.value = Some(default);
            self.variation_index = None;
            // N.B. reason remains untouched: this is counterintuitive, but consistent with Go
        }
        self
    }

    pub fn or_else<F>(mut self, default: F) -> Detail<T>
    where
        F: Fn() -> T,
    {
        if self.value.is_none() {
            self.value = Some(default());
            self.variation_index = None;
        }
        self
    }
}

/// Reason describes the reason that a flag evaluation produced a particular value.
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE", tag = "kind")]
pub enum Reason {
    /// Off indicates that the flag was off and therefore returned its configured off value.
    Off,
    /// TargetMatch indicates that the user key was specifically targeted for this flag.
    TargetMatch,
    /// RuleMatch indicates that the user matched one of the flag's rules.
    #[serde(rename_all = "camelCase")]
    RuleMatch {
        rule_index: usize,
        #[serde(skip_serializing_if = "String::is_empty")]
        rule_id: String,
        #[serde(skip_serializing_if = "std::ops::Not::not")]
        in_experiment: bool,
    },
    /// PrerequisiteFailed indicates that the flag was considered off because it had at
    /// least one prerequisite flag that either was off or did not return the desired variation.
    #[serde(rename_all = "camelCase")]
    PrerequisiteFailed { prerequisite_key: String },
    /// Fallthrough indicates that the flag was on but the user did not match any targets
    /// or rules.
    #[serde(rename_all = "camelCase")]
    Fallthrough {
        #[serde(skip_serializing_if = "std::ops::Not::not")]
        in_experiment: bool,
    },
    /// Error indicates that the flag could not be evaluated, e.g. because it does not
    /// exist or due to an unexpected error. In this case the result value will be the default value
    /// that the caller passed to the client.
    Error {
        #[serde(rename = "errorKind")]
        error: Error,
    },
}

impl Reason {
    pub fn is_in_experiment(&self) -> bool {
        match self {
            Reason::RuleMatch { in_experiment, .. } => *in_experiment,
            Reason::Fallthrough { in_experiment } => *in_experiment,
            _ => false,
        }
    }
}

/// Error is returned via a [`Reason::Error`] when the client could not evaluate a flag, and
/// provides information about why the flag could not be evaluated.
#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum Error {
    /// ClientNotReady indicates that the caller tried to evaluate a flag before the client
    /// had successfully initialized.
    ClientNotReady,
    /// FlagNotFound indicates that the caller provided a flag key that did not match any
    /// known flag.
    FlagNotFound,
    /// MalformedFlag indicates that there was an internal inconsistency in the flag data,
    /// e.g. a rule specified a nonexistent variation.
    MalformedFlag,
    /// WrongType indicates that the result value was not of the requested type, e.g. you
    /// called BoolVariationDetail but the value was an integer.
    WrongType,
    /// Exception indicates that an unexpected error stopped flag evaluation; check the
    /// log for details.
    Exception,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::flag_value::FlagValue::{Bool, Str};
    use crate::test_common::{InMemoryPrerequisiteEventRecorder, TestStore};
    use crate::variation::VariationOrRollout;
    use maplit::hashmap;
    use spectral::prelude::*;
    use std::cell::RefCell;

    #[test]
    fn test_eval_flag_basic() {
        let store = TestStore::new();
        let alice = User::with_key("alice").build(); // not targeted
        let bob = User::with_key("bob").build(); // targeted
        let mut flag = store.flag("flagWithTarget").unwrap().clone();

        assert!(!flag.on);
        let detail = evaluate(&store, &flag, &alice, None);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.variation_index).contains_value(0);
        assert_that!(detail.reason).is_equal_to(&Reason::Off);

        assert_that!(evaluate(&store, &flag, &bob, None)).is_equal_to(&detail);

        // flip off variation
        flag.off_variation = Some(1);
        let detail = evaluate(&store, &flag, &alice, None);
        assert_that!(detail.value).contains_value(&Bool(true));
        assert_that!(detail.variation_index).contains_value(1);

        // off variation unspecified
        flag.off_variation = None;
        let detail = evaluate(&store, &flag, &alice, None);
        assert_that!(detail.value).is_none();
        assert_that!(detail.variation_index).is_none();
        assert_that!(detail.reason).is_equal_to(&Reason::Off);

        // flip targeting on
        flag.on = true;
        let detail = evaluate(&store, &flag, &alice, None);
        assert_that!(detail.value).contains_value(&Bool(true));
        assert_that!(detail.variation_index).contains_value(1);
        assert_that!(detail.reason).is_equal_to(&Reason::Fallthrough {
            in_experiment: false,
        });

        let detail = evaluate(&store, &flag, &bob, None);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.variation_index).contains_value(0);
        assert_that!(detail.reason).is_equal_to(&Reason::TargetMatch);

        // flip default variation
        flag.fallthrough = VariationOrRollout::Variation { variation: 0 };
        let detail = evaluate(&store, &flag, &alice, None);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.variation_index).contains_value(0);

        // bob's reason should still be TargetMatch even though his value is now the default
        let detail = evaluate(&store, &flag, &bob, None);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.variation_index).contains_value(0);
        assert_that!(detail.reason).is_equal_to(&Reason::TargetMatch);
    }

    #[test]
    fn test_prerequisite_events_are_captured() {
        let recorder = InMemoryPrerequisiteEventRecorder {
            events: RefCell::new(Vec::new()),
        };
        let store = TestStore::new();
        let alice = User::with_key("alice").build();
        let flag = store.flag("flagWithNestedPrereq").unwrap().clone();

        let _ = evaluate(&store, &flag, &alice, Some(&recorder));
        assert_that!(*recorder.events.borrow()).has_length(2);

        let event = &recorder.events.borrow()[0];
        assert_eq!("flagWithSatisfiedPrereq", event.target_flag_key);
        assert_eq!("prereq", event.prerequisite_flag.key);

        let event = &recorder.events.borrow()[1];
        assert_eq!("flagWithNestedPrereq", event.target_flag_key);
        assert_eq!("flagWithSatisfiedPrereq", event.prerequisite_flag.key);
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
            let detail = evaluate(&store, &flag, user, None);
            assert_that!(detail.value).contains_value(&Bool(false));
            assert_that!(detail.variation_index).contains_value(0);
            assert_that!(detail.reason).is_equal_to(&Reason::Off);
        }

        // flip targeting on
        flag.on = true;
        let detail = evaluate(&store, &flag, &alice, None);
        assert_that!(detail.value).contains_value(&Bool(true));
        assert_that!(detail.variation_index).contains_value(1);
        assert_that!(detail.reason).is_equal_to(&Reason::Fallthrough {
            in_experiment: false,
        });

        let detail = evaluate(&store, &flag, &bob, None);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.variation_index).contains_value(0);
        assert_that!(detail.reason).is_equal_to(&Reason::RuleMatch {
            rule_id: "in-rule".to_string(),
            rule_index: 0,
            in_experiment: false,
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
            let detail = evaluate(&store, &flag, user, None);
            assert_that!(detail.value).contains_value(&Bool(false));
            assert_that!(detail.reason).is_equal_to(&Reason::PrerequisiteFailed {
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

        let detail = evaluate(&store, &flag, &alice, None);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.reason).is_equal_to(&Reason::PrerequisiteFailed {
            prerequisite_key: "offPrereq".to_string(),
        });
    }

    #[test]
    fn test_eval_flag_satisfied_prereq() {
        let mut store = TestStore::new();
        let flag = store.flag("flagWithSatisfiedPrereq").unwrap().clone();

        let alice = User::with_key("alice").build();
        let bob = User::with_key("bob").build();

        let detail = evaluate(&store, &flag, &alice, None);
        asserting!("alice should pass prereq and see fallthrough")
            .that(&detail.value)
            .contains_value(&Bool(true));
        let detail = evaluate(&store, &flag, &bob, None);
        asserting!("bob should see prereq failed due to target")
            .that(&detail.value)
            .contains_value(&Bool(false));
        assert_that!(detail.reason).is_equal_to(Reason::PrerequisiteFailed {
            prerequisite_key: "prereq".to_string(),
        });

        // prerequisite off
        store.update_flag("prereq", |flag| (flag.on = false));
        for user in vec![&alice, &bob] {
            let detail = evaluate(&store, &flag, user, None);
            assert_that!(detail.value).contains_value(&Bool(false));
            assert_that!(detail.reason).is_equal_to(&Reason::PrerequisiteFailed {
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

        let detail = evaluate(&store, &flag, &alice, None);
        asserting!("alice is in segment, should see false with RuleMatch")
            .that(&detail.value)
            .contains_value(&Bool(false));
        assert_that!(detail.reason).is_equal_to(Reason::RuleMatch {
            rule_id: "match-rule".to_string(),
            rule_index: 0,
            in_experiment: false,
        });
        let detail = evaluate(&store, &flag, &bob, None);
        asserting!("bob is not in segment and should see fallthrough")
            .that(&detail.value)
            .contains_value(&Bool(true));
        assert_that!(detail.reason).is_equal_to(Reason::Fallthrough {
            in_experiment: false,
        });
    }

    #[test]
    fn test_rollout_flag() {
        let store = TestStore::new();
        let flag = store.flag("flagWithRolloutBucketBy").unwrap();

        let alice = User::with_key("anonymous")
            .custom(hashmap! {
                "platform".into() => "aem".into(),
                "ld_quid".into() => "d4ad12cb-392b-4fce-b214-843ad625d6f8".into()
            })
            .build();

        let detail = evaluate(&store, &flag, &alice, None);
        assert_that!(detail.value).contains_value(&Str("rollout1".to_string()));
    }

    #[test]
    fn test_experiment_flag() {
        let store = TestStore::new();
        let flag = store.flag("flagWithExperiment").unwrap();

        let user_a = User::with_key("userKeyA").build();
        let detail = evaluate(&store, &flag, &user_a, None);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert!(detail.reason.is_in_experiment());

        let user_b = User::with_key("userKeyB").build();
        let detail = evaluate(&store, &flag, &user_b, None);
        assert_that!(detail.value).contains_value(&Bool(true));
        assert!(detail.reason.is_in_experiment());

        let user_c = User::with_key("userKeyC").build();
        let detail = evaluate(&store, &flag, &user_c, None);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert!(!detail.reason.is_in_experiment());
    }

    #[test]
    fn test_malformed_rule() {
        let store = TestStore::new();
        let mut flag = store.flag("flagWithMalformedRule").unwrap().clone();

        let user_a = User::with_key("no").build();
        let user_b = User::with_key("yes").build();

        let detail = evaluate(&store, &flag, &user_a, None);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.reason).is_equal_to(Reason::Off);

        let detail = evaluate(&store, &flag, &user_b, None);
        assert_that!(detail.value).contains_value(&Bool(false));
        assert_that!(detail.reason).is_equal_to(Reason::Off);

        flag.on = true;

        let detail = evaluate(&store, &flag, &user_a, None);
        assert_that!(detail.value).contains_value(&Bool(true));
        assert_that!(detail.reason).is_equal_to(Reason::Fallthrough {
            in_experiment: false,
        });

        let detail = evaluate(&store, &flag, &user_b, None);
        assert_that!(detail.value).is_none();
        assert_that!(detail.reason).is_equal_to(Reason::Error {
            error: Error::MalformedFlag,
        });
    }

    #[test]
    fn reason_serialization() {
        struct Case<'a> {
            reason: Reason,
            json: &'a str,
        }

        let cases = vec![
            Case {
                reason: Reason::Off,
                json: r#"{"kind":"OFF"}"#,
            },
            Case {
                reason: Reason::Fallthrough {
                    in_experiment: false,
                },
                json: r#"{"kind":"FALLTHROUGH"}"#,
            },
            Case {
                reason: Reason::Fallthrough {
                    in_experiment: true,
                },
                json: r#"{"kind":"FALLTHROUGH","inExperiment":true}"#,
            },
            Case {
                reason: Reason::TargetMatch {},
                json: r#"{"kind":"TARGET_MATCH"}"#,
            },
            Case {
                reason: Reason::RuleMatch {
                    rule_index: 1,
                    rule_id: "x".into(),
                    in_experiment: false,
                },
                json: r#"{"kind":"RULE_MATCH","ruleIndex":1,"ruleId":"x"}"#,
            },
            Case {
                reason: Reason::RuleMatch {
                    rule_index: 1,
                    rule_id: "x".into(),
                    in_experiment: true,
                },
                json: r#"{"kind":"RULE_MATCH","ruleIndex":1,"ruleId":"x","inExperiment":true}"#,
            },
            Case {
                reason: Reason::PrerequisiteFailed {
                    prerequisite_key: "x".into(),
                },
                json: r#"{"kind":"PREREQUISITE_FAILED","prerequisiteKey":"x"}"#,
            },
            Case {
                reason: Reason::Error {
                    error: Error::WrongType,
                },
                json: r#"{"kind":"ERROR","errorKind":"WRONG_TYPE"}"#,
            },
        ];

        for Case {
            reason,
            json: expected_json,
        } in cases
        {
            let json = serde_json::to_string(&reason).unwrap();
            assert_eq!(
                expected_json, json,
                "unexpected serialization: {:?}",
                reason
            );
        }
    }
}
