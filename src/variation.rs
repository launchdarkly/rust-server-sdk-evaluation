use serde::Deserialize;

use crate::{user::User, BucketPrefix};

pub type VariationIndex = usize;

#[derive(Debug, PartialEq)]
pub(crate) struct BucketResult {
    pub variation_index: VariationIndex,
    pub in_experiment: bool,
}

impl From<&VariationIndex> for BucketResult {
    fn from(variation_index: &VariationIndex) -> Self {
        BucketResult {
            variation_index: *variation_index,
            in_experiment: false, // single variations are never in an experiment
        }
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum VariationOrRollout {
    Variation(VariationIndex),
    Rollout(Rollout),
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum RolloutKind {
    Rollout,
    Experiment,
}

impl Default for RolloutKind {
    fn default() -> Self {
        RolloutKind::Rollout
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Rollout {
    #[serde(default)]
    kind: RolloutKind,
    bucket_by: Option<String>,
    variations: Vec<WeightedVariation>,
    seed: Option<i64>,
}

impl Rollout {
    #[cfg(test)]
    fn with_variations<V: Into<Vec<WeightedVariation>>>(variations: V) -> Self {
        Rollout {
            kind: RolloutKind::Rollout,
            bucket_by: None,
            seed: None,
            variations: variations.into(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(untagged)]
pub(crate) enum VariationOrRolloutOrMalformed {
    VariationOrRollout(VariationOrRollout),
    Malformed(serde_json::Value),
}

pub(crate) type VariationWeight = f32;

#[derive(Clone, Debug, Deserialize, PartialEq)]
pub struct WeightedVariation {
    pub variation: VariationIndex,
    pub weight: VariationWeight,
    #[serde(default)]
    pub untracked: bool,
}

impl WeightedVariation {
    #[cfg(test)]
    fn new(variation: VariationIndex, weight: VariationWeight) -> Self {
        WeightedVariation {
            variation,
            weight,
            untracked: false,
        }
    }

    fn as_bucket_result(&self, is_experiment: bool) -> BucketResult {
        BucketResult {
            variation_index: self.variation,
            in_experiment: is_experiment && !self.untracked,
        }
    }
}

impl VariationOrRollout {
    pub(crate) fn variation(
        &self,
        flag_key: &str,
        user: &User,
        salt: &str,
    ) -> Option<BucketResult> {
        match self {
            VariationOrRollout::Variation(variation) => Some(variation.into()),
            VariationOrRollout::Rollout(Rollout {
                kind,
                bucket_by,
                variations,
                seed,
            }) => {
                let is_experiment = kind == &RolloutKind::Experiment;

                let prefix = match seed {
                    Some(seed) => BucketPrefix::Seed(*seed),
                    None => BucketPrefix::KeyAndSalt(flag_key, salt),
                };

                let bucket = user.bucket(bucket_by.as_deref(), prefix);
                let mut sum = 0.0;
                for variation in variations {
                    sum += variation.weight / 100_000.0;
                    if bucket < sum {
                        return Some(variation.as_bucket_result(is_experiment));
                    }
                }
                variations
                    .last()
                    .map(|var| var.as_bucket_result(is_experiment))
            }
        }
    }
}

impl VariationOrRolloutOrMalformed {
    #[cfg(test)]
    fn get(&self) -> Result<&VariationOrRollout, String> {
        match self {
            VariationOrRolloutOrMalformed::VariationOrRollout(v) => Ok(v),
            VariationOrRolloutOrMalformed::Malformed(v) => {
                Err(format!("malformed variation_or_rollout: {}", v))
            }
        }
    }
}

impl From<VariationOrRollout> for VariationOrRolloutOrMalformed {
    fn from(vor: VariationOrRollout) -> VariationOrRolloutOrMalformed {
        VariationOrRolloutOrMalformed::VariationOrRollout(vor)
    }
}

#[cfg(test)]
mod tests {
    use crate::user::User;

    use super::*;
    use spectral::prelude::*;

    #[test]
    fn test_parse_variation_or_rollout() {
        let variation: VariationOrRolloutOrMalformed =
            serde_json::from_str(r#"{"variation":4}"#).expect("should parse");

        assert_that!(variation.get()).is_ok_containing(&VariationOrRollout::Variation(4));

        let rollout: VariationOrRolloutOrMalformed =
            serde_json::from_str(r#"{"rollout":{"variations":[{"variation":1,"weight":100000}]}}"#)
                .expect("should parse");
        assert_that!(rollout.get()).is_ok_containing(&VariationOrRollout::Rollout(
            Rollout::with_variations(vec![WeightedVariation::new(1, 100000.0)]),
        ));

        let rollout_bucket_by: VariationOrRolloutOrMalformed = serde_json::from_str(
            r#"{"rollout":{"bucketBy":"bucket","variations":[{"variation":1,"weight":100000}]}}"#,
        )
        .expect("should parse");
        assert_that!(rollout_bucket_by.get()).is_ok_containing(&VariationOrRollout::Rollout(
            Rollout {
                bucket_by: Some("bucket".to_string()),
                ..Rollout::with_variations(vec![WeightedVariation::new(1, 100000.0)])
            },
        ));

        let rollout_seed: VariationOrRolloutOrMalformed = serde_json::from_str(
            r#"{"rollout":{"variations":[{"variation":1,"weight":100000}],"seed":42}}"#,
        )
        .expect("should parse");
        assert_that!(rollout_seed.get()).is_ok_containing(&VariationOrRollout::Rollout(Rollout {
            seed: Some(42),
            ..Rollout::with_variations(vec![WeightedVariation::new(1, 100000.0)])
        }));

        let rollout_experiment: VariationOrRolloutOrMalformed = serde_json::from_str(
            r#"{
                 "rollout":
                   {
                     "kind": "experiment",
                     "variations": [
                       {"variation":1, "weight":20000},
                       {"variation":0, "weight":20000},
                       {"variation":0, "weight":60000, "untracked": true}
                     ],
                     "seed":42
                   }
            }"#,
        )
        .expect("should parse");
        assert_that!(rollout_experiment.get()).is_ok_containing(&VariationOrRollout::Rollout(
            Rollout {
                kind: RolloutKind::Experiment,
                seed: Some(42),
                ..Rollout::with_variations(vec![
                    WeightedVariation::new(1, 20000.0),
                    WeightedVariation::new(0, 20000.0),
                    WeightedVariation {
                        untracked: true,
                        ..WeightedVariation::new(0, 60000.0)
                    },
                ])
            },
        ));

        let malformed: VariationOrRolloutOrMalformed =
            serde_json::from_str("{}").expect("should parse");
        assert_that!(malformed.get()).is_err();
    }

    #[test]
    fn variation_index_for_user() {
        const HASH_KEY: &str = "hashKey";
        const SALT: &str = "saltyA";

        let wv0 = WeightedVariation::new(0, 60_000.0);
        let wv1 = WeightedVariation::new(1, 40_000.0);
        let rollout = VariationOrRollout::Rollout(Rollout::with_variations(vec![wv0, wv1]));

        asserting!("userKeyA (bucket 0.42157587) should get variation 0")
            .that(&rollout.variation(HASH_KEY, &User::with_key("userKeyA").build(), SALT))
            .contains_value(BucketResult {
                variation_index: 0,
                in_experiment: false,
            });
        asserting!("userKeyB (bucket 0.6708485) should get variation 1")
            .that(&rollout.variation(HASH_KEY, &User::with_key("userKeyB").build(), SALT))
            .contains_value(BucketResult {
                variation_index: 1,
                in_experiment: false,
            });
        asserting!("userKeyC (bucket 0.10343106) should get variation 0")
            .that(&rollout.variation(HASH_KEY, &User::with_key("userKeyC").build(), SALT))
            .contains_value(BucketResult {
                variation_index: 0,
                in_experiment: false,
            });
    }

    #[test]
    fn variation_index_for_user_in_experiment() {
        const HASH_KEY: &str = "hashKey";
        const SALT: &str = "saltyA";

        let wv0 = WeightedVariation {
            variation: 0,
            weight: 10_000.0,
            untracked: false,
        };
        let wv1 = WeightedVariation {
            variation: 1,
            weight: 20_000.0,
            untracked: false,
        };
        let wv0_untracked = WeightedVariation {
            variation: 0,
            weight: 70_000.0,
            untracked: true,
        };
        let rollout = VariationOrRollout::Rollout(Rollout {
            seed: Some(61),
            kind: RolloutKind::Experiment,
            ..Rollout::with_variations(vec![wv0, wv1, wv0_untracked])
        });

        asserting!("userKeyA (bucket 0.09801207) should get variation 0 and be in the experiment")
            .that(&rollout.variation(HASH_KEY, &User::with_key("userKeyA").build(), SALT))
            .contains_value(BucketResult {
                variation_index: 0,
                in_experiment: true,
            });
        asserting!("userKeyB (bucket 0.14483777) should get variation 1 and be in the experiment")
            .that(&rollout.variation(HASH_KEY, &User::with_key("userKeyB").build(), SALT))
            .contains_value(BucketResult {
                variation_index: 1,
                in_experiment: true,
            });
        asserting!(
            "userKeyC (bucket 0.9242641) should get variation 0 and not be in the experiment"
        )
        .that(&rollout.variation(HASH_KEY, &User::with_key("userKeyC").build(), SALT))
        .contains_value(BucketResult {
            variation_index: 0,
            in_experiment: false,
        });
    }

    #[test]
    fn incomplete_weighting_defaults_to_last_variation() {
        const HASH_KEY: &str = "hashKey";
        const SALT: &str = "saltyA";

        let wv0 = WeightedVariation::new(0, 1.0);
        let wv1 = WeightedVariation::new(1, 2.0);
        let wv2 = WeightedVariation::new(2, 3.0);
        let rollout = VariationOrRollout::Rollout(Rollout::with_variations(vec![wv0, wv1, wv2]));

        asserting!("userKeyD (bucket 0.7816281) should get variation 2")
            .that(&rollout.variation(HASH_KEY, &User::with_key("userKeyD").build(), SALT))
            .contains_value(BucketResult {
                variation_index: 2,
                in_experiment: false,
            });
    }
}
