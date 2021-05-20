use serde::Deserialize;

use crate::user::User;

pub type VariationIndex = usize;

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum VariationOrRollout {
    Variation(VariationIndex),
    Rollout(Rollout),
}
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Rollout {
    bucket_by: Option<String>,
    variations: Vec<WeightedVariation>,
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(untagged)]
pub(crate) enum VariationOrRolloutOrMalformed {
    VariationOrRollout(VariationOrRollout),
    Malformed(serde_json::Value),
}

type VariationWeight = f32;

#[derive(Clone, Debug, Deserialize, PartialEq)]
pub struct WeightedVariation {
    pub variation: VariationIndex,
    pub weight: VariationWeight,
}

impl VariationOrRollout {
    pub fn variation(&self, flag_key: &str, user: &User, salt: &str) -> Option<VariationIndex> {
        match self {
            VariationOrRollout::Variation(index) => Some(*index),
            VariationOrRollout::Rollout(Rollout {
                bucket_by,
                variations,
            }) => {
                let bucket = user.bucket(flag_key, bucket_by.as_ref().map(String::as_str), salt);
                let mut sum = 0.0;
                for variation in variations {
                    sum += variation.weight / 100_000.0;
                    if bucket < sum {
                        return Some(variation.variation);
                    }
                }
                None
            }
        }
    }
}

impl VariationOrRolloutOrMalformed {
    pub fn get(&self) -> Result<&VariationOrRollout, String> {
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

    use super::{Rollout, VariationOrRollout, VariationOrRolloutOrMalformed, WeightedVariation};
    use spectral::prelude::*;

    #[test]
    fn test_parse_variation_or_rollout() {
        let variation: VariationOrRolloutOrMalformed =
            serde_json::from_str(r#"{"variation":4}"#).expect("should parse");
        assert_that!(variation.get()).is_ok_containing(&VariationOrRollout::Variation(4));

        let rollout: VariationOrRolloutOrMalformed =
            serde_json::from_str(r#"{"rollout":{"variations":[{"variation":1,"weight":100000}]}}"#)
                .expect("should parse");
        assert_that!(rollout.get()).is_ok_containing(&VariationOrRollout::Rollout(Rollout {
            bucket_by: None,
            variations: vec![WeightedVariation {
                variation: 1,
                weight: 100000.0,
            }],
        }));

        let rollout: VariationOrRolloutOrMalformed = serde_json::from_str(
            r#"{"rollout":{"bucketBy":"bucket","variations":[{"variation":1,"weight":100000}]}}"#,
        )
        .expect("should parse");
        assert_that!(rollout.get()).is_ok_containing(&VariationOrRollout::Rollout(Rollout {
            bucket_by: Some("bucket".to_string()),
            variations: vec![WeightedVariation {
                variation: 1,
                weight: 100000.0,
            }],
        }));

        let malformed: VariationOrRolloutOrMalformed =
            serde_json::from_str("{}").expect("should parse");
        assert_that!(malformed.get()).is_err();
    }

    #[test]
    fn variation_index_for_user() {
        const HASH_KEY: &str = "hashKey";
        const SALT: &str = "saltyA";

        let wv1 = WeightedVariation {
            variation: 0,
            weight: 60_000.0,
        };
        let wv2 = WeightedVariation {
            variation: 1,
            weight: 40_000.0,
        };
        let rollout = VariationOrRollout::Rollout(Rollout {
            bucket_by: None,
            variations: vec![wv1, wv2],
        });

        asserting!("userKeyA should get variation 0")
            .that(&rollout.variation(HASH_KEY, &User::with_key("userKeyA").build(), SALT))
            .contains_value(0);
        asserting!("userKeyB should get variation 1")
            .that(&rollout.variation(HASH_KEY, &User::with_key("userKeyB").build(), SALT))
            .contains_value(1);
        asserting!("userKeyC should get variation 0")
            .that(&rollout.variation(HASH_KEY, &User::with_key("userKeyC").build(), SALT))
            .contains_value(0);
    }
}
