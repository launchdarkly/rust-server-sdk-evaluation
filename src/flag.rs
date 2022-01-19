use std::fmt;

use serde::de::{MapAccess, Visitor};
use serde::{Deserialize, Deserializer};

use crate::eval::{self, Detail, Reason};
use crate::flag_value::FlagValue;
use crate::rule::FlagRule;
use crate::user::User;
use crate::variation::{VariationIndex, VariationOrRollout};
use crate::BucketResult;

/// Flag describes an individual feature flag.
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Flag {
    /// The unique string key of the feature flag.
    pub key: String,

    /// Version is an integer that is incremented by LaunchDarkly every time the configuration of the flag is
    /// changed.
    #[serde(default)]
    pub version: u64,

    pub(crate) on: bool,

    pub(crate) targets: Vec<Target>,
    pub(crate) rules: Vec<FlagRule>,
    pub(crate) prerequisites: Vec<Prereq>,

    pub(crate) fallthrough: VariationOrRollout,
    pub(crate) off_variation: Option<VariationIndex>,
    variations: Vec<FlagValue>,

    /// Indicates whether a flag is available using each of the client-side authentication methods.
    #[serde(flatten)]
    client_visibility: ClientVisibility,

    salt: String,

    /// Used internally by the SDK analytics event system.
    ///
    /// This field is true if the current LaunchDarkly account has data export enabled, and has turned on
    /// the "send detailed event information for this flag" option for this flag. This tells the SDK to
    /// send full event data for each flag evaluation, rather than only aggregate data in a summary event.
    ///
    /// The launchdarkly-server-sdk-evaluation crate does not implement that behavior; it is only
    /// in the data model for use by the SDK.
    #[serde(default)]
    pub track_events: bool,

    /// Used internally by the SDK analytics event system.
    ///
    /// This field is true if the current LaunchDarkly account has experimentation enabled, has associated
    /// this flag with an experiment, and has enabled "default rule" for the experiment. This tells the
    /// SDK to send full event data for any evaluation where this flag had targeting turned on but the
    /// user did not match any targets or rules.
    ///
    /// The launchdarkly-server-sdk-evaluation package does not implement that behavior; it is only
    /// in the data model for use by the SDK.
    #[serde(default)]
    pub track_events_fallthrough: bool,

    /// Used internally by the SDK analytics event system.
    ///
    /// This field is non-zero if debugging for this flag has been turned on temporarily in the
    /// LaunchDarkly dashboard. Debugging always is for a limited time, so the field specifies a Unix
    /// millisecond timestamp when this mode should expire. Until then, the SDK will send full event data
    /// for each evaluation of this flag.
    ///
    /// The launchdarkly-server-sdk-evaluation package does not implement that behavior; it is only in the data
    /// model for use by the SDK.
    #[serde(default)]
    pub debug_events_until_date: Option<u64>,
}

// This struct exists only so we can add some custom deserialization logic to account for the
// potential presence of a client_side field in lieu of the client_side_availability field.
#[derive(Clone, Debug)]
struct ClientVisibility {
    client_side_availability: ClientSideAvailability,
}

impl<'de> Deserialize<'de> for ClientVisibility {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "camelCase")]
        enum Field {
            ClientSide,
            ClientSideAvailability,
        }

        struct ClientVisibilityVisitor;

        impl<'de> Visitor<'de> for ClientVisibilityVisitor {
            type Value = ClientVisibility;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct ClientVisibility")
            }

            fn visit_map<V>(self, mut map: V) -> Result<ClientVisibility, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut client_side = None;
                let mut client_side_availability: Option<ClientSideAvailability> = None;

                while let Some(k) = map.next_key()? {
                    match k {
                        Field::ClientSide => client_side = Some(map.next_value()?),
                        Field::ClientSideAvailability => {
                            client_side_availability = Some(map.next_value()?)
                        }
                    }
                }

                let client_side_availability = match client_side_availability {
                    Some(mut csa) => {
                        csa.explicit = true;
                        csa
                    }
                    _ => ClientSideAvailability {
                        using_environment_id: client_side.unwrap_or_default(),
                        using_mobile_key: true,
                        explicit: false,
                    },
                };

                Ok(ClientVisibility {
                    client_side_availability,
                })
            }
        }

        const FIELDS: &[&str] = &["clientSide", "clientSideAvailability"];
        deserializer.deserialize_struct("ClientVisibility", FIELDS, ClientVisibilityVisitor)
    }
}

/// Prereq describes a requirement that another feature flag return a specific variation.
///
/// A prerequisite condition is met if the specified prerequisite flag has targeting turned on and
/// returns the specified variation.
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

/// ClientSideAvailability describes whether a flag is available to client-side SDKs.
///
/// This field can be used by a server-side client to determine whether to include an individual flag in
/// bootstrapped set of flag data (see <https://docs.launchdarkly.com/sdk/client-side/javascript#bootstrapping>).
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ClientSideAvailability {
    /// Indicates that this flag is available to clients using the mobile key for
    /// authorization (includes most desktop and mobile clients).
    pub using_mobile_key: bool,
    /// Indicates that this flag is available to clients using the environment
    /// id to identify an environment (includes client-side javascript clients).
    pub using_environment_id: bool,

    // This field determines if ClientSideAvailability was explicitly included in the JSON payload.
    //
    // If it was, we will use the properities of this new schema over the dated
    // [ClientVisibility::client_side] field.
    #[serde(skip)]
    explicit: bool,
}

impl Flag {
    /// Generate a [crate::Detail] response with the given variation and reason.
    pub fn variation(&self, index: VariationIndex, reason: Reason) -> Detail<&FlagValue> {
        Detail {
            value: self.variations.get(index),
            variation_index: Some(index),
            reason,
        }
        .should_have_value(eval::Error::MalformedFlag)
    }

    /// Generate a [crate::Detail] response using the flag's off variation.
    ///
    /// If a flag has an off_variation specified, a [crate::Detail] will be created using that
    /// variation. If the flag does not have an off_variation specified, an empty [crate::Detail]
    /// will be returned. See [crate::Detail::empty].
    pub fn off_value(&self, reason: Reason) -> Detail<&FlagValue> {
        match self.off_variation {
            Some(index) => self.variation(index, reason),
            None => Detail::empty(reason),
        }
    }

    /// Indicates that this flag is available to clients using the environment id to identify an
    /// environment (includes client-side javascript clients).
    pub fn using_environment_id(&self) -> bool {
        self.client_visibility
            .client_side_availability
            .using_environment_id
    }

    /// Indicates that this flag is available to clients using the mobile key for authorization
    /// (includes most desktop and mobile clients).
    pub fn using_mobile_key(&self) -> bool {
        self.client_visibility
            .client_side_availability
            .using_mobile_key
    }

    pub(crate) fn resolve_variation_or_rollout(
        &self,
        vr: &VariationOrRollout,
        user: &User,
    ) -> Result<BucketResult, eval::Error> {
        vr.variation(&self.key, user, &self.salt)
            .ok_or(eval::Error::MalformedFlag)
    }

    /// Returns true if, based on the [crate::Reason] returned by the flag evaluation, an event for
    /// that evaluation should have full tracking enabled and always report the reason even if the
    /// application didn't explicitly request this. For instance, this is true if a rule was
    /// matched that had tracking enabled for that specific rule.
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
            on: true,
            targets: vec![],
            rules: vec![crate::rule::FlagRule::new_segment_match(segment_keys)],
            prerequisites: vec![],
            fallthrough: VariationOrRollout::Variation { variation: 0 },
            off_variation: Some(0),
            variations: vec![FlagValue::Bool(false), FlagValue::Bool(true)],
            client_visibility: ClientVisibility {
                client_side_availability: ClientSideAvailability {
                    using_mobile_key: false,
                    using_environment_id: false,
                    explicit: true,
                },
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
    use crate::store::Store;
    use crate::test_common::TestStore;
    use spectral::prelude::*;

    use super::Flag;
    use crate::eval::Reason::*;
    use test_case::test_case;

    #[test_case(true)]
    #[test_case(false)]
    fn handles_old_flag_schema(client_side: bool) {
        let json = &format!(
            r#"{{
            "key": "flag",
            "version": 42,
            "on": false,
            "targets": [],
            "rules": [],
            "prerequisites": [],
            "fallthrough": {{"variation": 1}},
            "offVariation": 0,
            "variations": [false, true],
            "clientSide": {},
            "salt": "salty"
        }}"#,
            client_side
        );

        let flag: Flag = serde_json::from_str(json).unwrap();
        let client_side_availability = &flag.client_visibility.client_side_availability;
        assert_eq!(client_side_availability.using_environment_id, client_side);
        assert!(client_side_availability.using_mobile_key);
        assert_eq!(client_side_availability.explicit, false);

        assert_eq!(flag.using_environment_id(), client_side);
    }

    #[test_case(true)]
    #[test_case(false)]
    fn handles_new_flag_schema(using_environment_id: bool) {
        let json = &format!(
            r#"{{
            "key": "flag",
            "version": 42,
            "on": false,
            "targets": [],
            "rules": [],
            "prerequisites": [],
            "fallthrough": {{"variation": 1}},
            "offVariation": 0,
            "variations": [false, true],
            "clientSideAvailability": {{
                "usingEnvironmentId": {},
                "usingMobileKey": false
            }},
            "salt": "salty"
        }}"#,
            using_environment_id
        );

        let flag: Flag = serde_json::from_str(json).unwrap();
        let client_side_availability = &flag.client_visibility.client_side_availability;
        assert_eq!(
            client_side_availability.using_environment_id,
            using_environment_id
        );
        assert!(!client_side_availability.using_mobile_key);
        assert_eq!(client_side_availability.explicit, true);

        assert_eq!(flag.using_environment_id(), using_environment_id);
    }

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
}
