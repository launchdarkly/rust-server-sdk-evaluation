use serde::Deserialize;

use crate::user::User;

pub type VariationIndex = usize;

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum VariationOrRollout {
    Variation(VariationIndex),
    Rollout {
        bucket_by: Option<String>,
        variations: Vec<WeightedVariation>,
    },
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
            VariationOrRollout::Rollout {
                bucket_by,
                variations,
            } => {
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
