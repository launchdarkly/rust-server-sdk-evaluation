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

    on: bool,

    targets: Vec<Target>,
    rules: Vec<FlagRule>,
    prerequisites: Vec<Prereq>,

    fallthrough: VariationOrRolloutOrMalformed,
    off_variation: Option<VariationIndex>,
    variations: Vec<FlagValue>,

    //#[serde(default)]
    //track_events: bool,
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
