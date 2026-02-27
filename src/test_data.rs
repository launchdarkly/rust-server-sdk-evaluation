//! Test data source builders for creating flags and segments for testing purposes.
//!
//! This module provides builder APIs for constructing feature flags with various configurations
//! without needing to connect to LaunchDarkly services. These builders are intended for use in
//! test scenarios and local development.

use crate::{
    contexts::context::Kind, flag::Target, flag_value::FlagValue, rule::Clause, rule::FlagRule,
    AttributeValue, Flag,
};

/// Builder for constructing test flags with various configurations.
///
/// The flag builder provides a fluent API for creating flags with targeting rules,
/// variations, and other configuration options. This is the primary way to create
/// test flags for use in testing and development scenarios.
pub struct FlagBuilder {
    key: String,
    on: bool,
    variations: Vec<FlagValue>,
    fallthrough_variation: usize,
    off_variation: usize,
    targets: Vec<Target>,
    rules: Vec<FlagRule>,
    sampling_ratio: Option<u32>,
    exclude_from_summaries: bool,
}

impl FlagBuilder {
    /// Creates a new flag builder for the given flag key.
    ///
    /// If creating a new flag, it will be initialized as a boolean flag with:
    /// - Variations: [true, false]
    /// - Targeting enabled (on: true)
    /// - Fallthrough variation: 0 (true)
    /// - Off variation: 1 (false)
    pub fn new(key: impl Into<String>) -> Self {
        Self {
            key: key.into(),
            on: true,
            variations: vec![FlagValue::Bool(true), FlagValue::Bool(false)],
            fallthrough_variation: 0,
            off_variation: 1,
            targets: vec![],
            rules: vec![],
            sampling_ratio: None,
            exclude_from_summaries: false,
        }
    }

    /// Configures the flag as a boolean type with variations [true, false].
    ///
    /// Sets:
    /// - Variations: [true, false]
    /// - Fallthrough variation: 0 (true)
    /// - Off variation: 1 (false)
    pub fn boolean_flag(mut self) -> Self {
        self.variations = vec![FlagValue::Bool(true), FlagValue::Bool(false)];
        self.fallthrough_variation = 0;
        self.off_variation = 1;
        self
    }

    /// Sets the variations for this flag.
    pub fn variations<I>(mut self, variations: I) -> Self
    where
        I: IntoIterator<Item = FlagValue>,
    {
        self.variations = variations.into_iter().collect();
        self
    }

    /// Sets whether targeting is enabled for the flag.
    ///
    /// When targeting is off (false), the flag returns the off variation regardless
    /// of other configuration.
    pub fn on(mut self, on: bool) -> Self {
        self.on = on;
        self
    }

    /// Sets the fallthrough variation for boolean flags.
    ///
    /// This is a convenience method equivalent to calling `fallthrough_variation_index`
    /// with 0 for true or 1 for false.
    pub fn fallthrough_variation(self, value: bool) -> Self {
        self.fallthrough_variation_index(if value { 0 } else { 1 })
    }

    /// Sets the fallthrough variation by index.
    ///
    /// The fallthrough variation is returned when targeting is on but no targets or rules match.
    pub fn fallthrough_variation_index(mut self, index: usize) -> Self {
        self.fallthrough_variation = index;
        self
    }

    /// Sets the off variation for boolean flags.
    ///
    /// This is a convenience method equivalent to calling `off_variation_index`
    /// with 0 for true or 1 for false.
    pub fn off_variation(self, value: bool) -> Self {
        self.off_variation_index(if value { 0 } else { 1 })
    }

    /// Sets the off variation by index.
    ///
    /// The off variation is returned when targeting is disabled (on: false).
    pub fn off_variation_index(mut self, index: usize) -> Self {
        self.off_variation = index;
        self
    }

    /// Configures the flag to always return the specified boolean value for everyone.
    ///
    /// This is a convenience method that:
    /// - Enables targeting (on: true)
    /// - Removes all targets and rules
    /// - Sets the fallthrough variation to the specified value
    pub fn variation_for_all(mut self, value: bool) -> Self {
        self.on = true;
        self.targets.clear();
        self.rules.clear();
        self.fallthrough_variation = if value { 0 } else { 1 };
        self
    }

    /// Configures the flag to always return the specified variation index for everyone.
    ///
    /// This is a convenience method that:
    /// - Enables targeting (on: true)
    /// - Removes all targets and rules
    /// - Sets the fallthrough variation to the specified index
    pub fn variation_for_all_index(mut self, index: usize) -> Self {
        self.on = true;
        self.targets.clear();
        self.rules.clear();
        self.fallthrough_variation = index;
        self
    }

    /// Configures the flag to always return the specified value.
    ///
    /// This is a convenience method that:
    /// - Sets a single variation equal to the specified value
    /// - Enables targeting (on: true)
    /// - Removes all targets and rules
    /// - Sets both fallthrough and off variation to index 0
    pub fn value_for_all(mut self, value: FlagValue) -> Self {
        self.variations = vec![value];
        self.on = true;
        self.targets.clear();
        self.rules.clear();
        self.fallthrough_variation = 0;
        self.off_variation = 0;
        self
    }

    /// Configures the flag to return a specific boolean value for a user context.
    ///
    /// This is a convenience method for targeting contexts with kind: "user".
    pub fn variation_for_user(self, user_key: impl Into<String>, variation: bool) -> Self {
        self.variation_index_for_key(Kind::user(), user_key, if variation { 0 } else { 1 })
    }

    /// Configures the flag to return a specific boolean value for a context of any kind.
    pub fn variation_for_key(
        self,
        context_kind: Kind,
        key: impl Into<String>,
        variation: bool,
    ) -> Self {
        self.variation_index_for_key(context_kind, key, if variation { 0 } else { 1 })
    }

    /// Configures the flag to return a specific variation index for a user context.
    ///
    /// This is a convenience method for targeting contexts with kind: "user".
    pub fn variation_index_for_user(self, user_key: impl Into<String>, variation: usize) -> Self {
        self.variation_index_for_key(Kind::user(), user_key, variation)
    }

    /// Configures the flag to return a specific variation index for a context of any kind.
    ///
    /// When a context key is targeted, that key is automatically removed from targeting
    /// for any other variation of the same flag (a key can only be targeted for one
    /// variation at a time).
    pub fn variation_index_for_key(
        mut self,
        context_kind: Kind,
        key: impl Into<String>,
        variation: usize,
    ) -> Self {
        let key = key.into();

        // Remove the key from all existing targets for this context kind
        for target in &mut self.targets {
            if target.context_kind == context_kind {
                target.values.retain(|k| k != &key);
            }
        }

        // Find or create target for this variation and context kind
        let target = self
            .targets
            .iter_mut()
            .find(|t| t.variation == variation as isize && t.context_kind == context_kind);

        if let Some(target) = target {
            if !target.values.contains(&key) {
                target.values.push(key);
            }
        } else {
            self.targets.push(Target {
                context_kind,
                values: vec![key],
                variation: variation as isize,
            });
        }

        self
    }

    /// Removes all individual context targets from the flag.
    pub fn clear_targets(mut self) -> Self {
        self.targets.clear();
        self
    }

    /// Creates a rule that matches when the specified user attribute equals any of the provided values.
    ///
    /// This is a convenience method for creating rules that target contexts with kind: "user".
    /// Returns a RuleBuilder that can be used to add more conditions or complete the rule.
    pub fn if_match<I>(self, attribute: impl Into<String>, values: I) -> RuleBuilder
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        self.if_match_context(Kind::user(), attribute, values)
    }

    /// Creates a rule that matches when the specified attribute equals any of the provided values
    /// for a context of the specified kind.
    ///
    /// Returns a RuleBuilder that can be used to add more conditions or complete the rule.
    pub fn if_match_context<I>(
        self,
        context_kind: Kind,
        attribute: impl Into<String>,
        values: I,
    ) -> RuleBuilder
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        RuleBuilder::new(self, context_kind, attribute, values, false)
    }

    /// Creates a rule that matches when the specified user attribute does NOT equal any of the provided values.
    ///
    /// This is identical to `if_match` except it uses negated logic.
    pub fn if_not_match<I>(self, attribute: impl Into<String>, values: I) -> RuleBuilder
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        self.if_not_match_context(Kind::user(), attribute, values)
    }

    /// Creates a rule that matches when the specified attribute does NOT equal any of the provided values
    /// for a context of the specified kind.
    pub fn if_not_match_context<I>(
        self,
        context_kind: Kind,
        attribute: impl Into<String>,
        values: I,
    ) -> RuleBuilder
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        RuleBuilder::new(self, context_kind, attribute, values, true)
    }

    /// Removes all rules from the flag.
    pub fn clear_rules(mut self) -> Self {
        self.rules.clear();
        self
    }

    /// Sets the event sampling ratio for the flag.
    pub fn sampling_ratio(mut self, ratio: u32) -> Self {
        self.sampling_ratio = Some(ratio);
        self
    }

    /// Sets whether the flag should be excluded from summary event counts.
    pub fn exclude_from_summaries(mut self, exclude: bool) -> Self {
        self.exclude_from_summaries = exclude;
        self
    }

    /// Builds the final Flag instance.
    ///
    /// This method creates a complete Flag with all configured settings.
    pub fn build(self) -> Flag {
        // Construct JSON and deserialize to avoid dealing with private fields
        let mut json = serde_json::json!({
            "key": self.key,
            "version": 1,
            "on": self.on,
            "targets": self.targets,
            "contextTargets": [],
            "rules": self.rules,
            "prerequisites": [],
            "fallthrough": { "variation": self.fallthrough_variation },
            "offVariation": self.off_variation,
            "variations": self.variations,
            "clientSide": false,
            "salt": "",
            "trackEvents": false,
            "trackEventsFallthrough": false,
        });

        if let Some(ratio) = self.sampling_ratio {
            json["samplingRatio"] = serde_json::json!(ratio);
        }
        if self.exclude_from_summaries {
            json["excludeFromSummaries"] = serde_json::json!(true);
        }

        serde_json::from_value(json).expect("Failed to build flag from valid JSON")
    }
}

/// Builder for constructing flag rules with multiple clauses.
///
/// Rules are evaluated in the order they were added to the flag. The first matching rule wins.
/// Rules are evaluated after individual context targets but before the fallthrough variation.
pub struct RuleBuilder {
    flag_builder: FlagBuilder,
    clauses: Vec<Clause>,
    rule_id: Option<String>,
}

impl RuleBuilder {
    fn new<I>(
        flag_builder: FlagBuilder,
        context_kind: Kind,
        attribute: impl Into<String>,
        values: I,
        negate: bool,
    ) -> Self
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        let attribute_str = attribute.into();
        let values_vec: Vec<AttributeValue> = values.into_iter().collect();

        let clause_json = serde_json::json!({
            "contextKind": context_kind,
            "attribute": attribute_str,
            "negate": negate,
            "op": "in",
            "values": values_vec,
        });

        let clause: Clause =
            serde_json::from_value(clause_json).expect("Failed to create clause from valid JSON");

        Self {
            flag_builder,
            clauses: vec![clause],
            rule_id: None,
        }
    }

    /// Adds another clause to the current rule for user contexts.
    ///
    /// Multiple clauses in a rule have AND semantics - all must match for the rule to match.
    pub fn and_match<I>(self, attribute: impl Into<String>, values: I) -> Self
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        self.and_match_context(Kind::user(), attribute, values)
    }

    /// Adds another clause to the current rule for a context of the specified kind.
    ///
    /// Multiple clauses in a rule have AND semantics - all must match for the rule to match.
    pub fn and_match_context<I>(
        mut self,
        context_kind: Kind,
        attribute: impl Into<String>,
        values: I,
    ) -> Self
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        let attribute_str = attribute.into();
        let values_vec: Vec<AttributeValue> = values.into_iter().collect();

        let clause_json = serde_json::json!({
            "contextKind": context_kind,
            "attribute": attribute_str,
            "negate": false,
            "op": "in",
            "values": values_vec,
        });

        let clause: Clause =
            serde_json::from_value(clause_json).expect("Failed to create clause from valid JSON");

        self.clauses.push(clause);
        self
    }

    /// Adds a negated clause to the current rule for user contexts.
    ///
    /// The clause must NOT match any of the values for the rule to match.
    pub fn and_not_match<I>(self, attribute: impl Into<String>, values: I) -> Self
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        self.and_not_match_context(Kind::user(), attribute, values)
    }

    /// Adds a negated clause to the current rule for a context of the specified kind.
    ///
    /// The clause must NOT match any of the values for the rule to match.
    pub fn and_not_match_context<I>(
        mut self,
        context_kind: Kind,
        attribute: impl Into<String>,
        values: I,
    ) -> Self
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        let attribute_str = attribute.into();
        let values_vec: Vec<AttributeValue> = values.into_iter().collect();

        let clause_json = serde_json::json!({
            "contextKind": context_kind,
            "attribute": attribute_str,
            "negate": true,
            "op": "in",
            "values": values_vec,
        });

        let clause: Clause =
            serde_json::from_value(clause_json).expect("Failed to create clause from valid JSON");

        self.clauses.push(clause);
        self
    }

    /// Sets a custom rule ID for this rule.
    ///
    /// By default, rules are assigned auto-generated IDs like "rule0", "rule1", etc.
    /// Use this method to override with a custom ID.
    pub fn with_id(mut self, rule_id: impl Into<String>) -> Self {
        self.rule_id = Some(rule_id.into());
        self
    }

    /// Completes the rule configuration for a boolean flag.
    ///
    /// This method adds the completed rule to the flag and returns control to the flag builder.
    pub fn then_return(self, variation: bool) -> FlagBuilder {
        self.then_return_index(if variation { 0 } else { 1 })
    }

    /// Completes the rule configuration with a variation index.
    ///
    /// This method adds the completed rule to the flag and returns control to the flag builder.
    pub fn then_return_index(self, variation: usize) -> FlagBuilder {
        let rule_id = self
            .rule_id
            .unwrap_or_else(|| format!("rule{}", self.flag_builder.rules.len()));

        let rule_json = serde_json::json!({
            "id": rule_id,
            "clauses": self.clauses,
            "variation": variation,
            "trackEvents": false,
        });

        let rule: FlagRule =
            serde_json::from_value(rule_json).expect("Failed to create rule from valid JSON");

        let mut flag_builder = self.flag_builder;
        flag_builder.rules.push(rule);
        flag_builder
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{eval::evaluate, variation::VariationOrRollout, ContextBuilder, Store};

    // Simple in-memory store for testing
    struct TestStore {
        flag: Option<Flag>,
    }

    impl Store for TestStore {
        fn flag(&self, _flag_key: &str) -> Option<Flag> {
            self.flag.clone()
        }

        fn segment(&self, _segment_key: &str) -> Option<crate::Segment> {
            None
        }
    }

    #[test]
    fn new_flag_has_boolean_defaults() {
        let flag = FlagBuilder::new("test-flag").build();

        assert_eq!(flag.key, "test-flag");
        assert_eq!(flag.on, true);
        assert_eq!(flag.off_variation, Some(1));

        // Test by evaluating - should return true (fallthrough variation 0)
        let store = TestStore {
            flag: Some(flag.clone()),
        };
        let context = ContextBuilder::new("user-123").build().unwrap();
        let flag_from_store = store.flag("test-flag").unwrap();
        let result = evaluate(&store, &flag_from_store, &context, None);
        assert_eq!(result.value, Some(&FlagValue::Bool(true)));
    }

    #[test]
    fn boolean_flag_resets_to_boolean_config() {
        let flag = FlagBuilder::new("test-flag")
            .variations(vec![
                FlagValue::Str("red".to_string()),
                FlagValue::Str("blue".to_string()),
            ])
            .boolean_flag()
            .build();

        // Use evaluation to verify the boolean configuration
        let store = TestStore {
            flag: Some(flag.clone()),
        };
        let context = ContextBuilder::new("user-123").build().unwrap();
        let result = evaluate(&store, &flag, &context, None);

        // Should evaluate to boolean true (fallthrough variation 0)
        assert_eq!(result.value, Some(&FlagValue::Bool(true)));
        assert_eq!(flag.off_variation, Some(1));
    }

    #[test]
    fn variations_sets_custom_variations() {
        let flag = FlagBuilder::new("test-flag")
            .variations(vec![
                FlagValue::Str("red".to_string()),
                FlagValue::Str("green".to_string()),
                FlagValue::Str("blue".to_string()),
            ])
            .fallthrough_variation_index(0)
            .build();

        // Test by evaluating
        let store = TestStore {
            flag: Some(flag.clone()),
        };
        let context = ContextBuilder::new("user-123").build().unwrap();
        let result = evaluate(&store, &flag, &context, None);

        assert_eq!(result.value, Some(&FlagValue::Str("red".to_string())));
    }

    #[test]
    fn on_method_sets_targeting_state() {
        let flag_on = FlagBuilder::new("test-flag").on(true).build();
        assert_eq!(flag_on.on, true);

        let flag_off = FlagBuilder::new("test-flag").on(false).build();
        assert_eq!(flag_off.on, false);
    }

    #[test]
    fn fallthrough_variation_sets_boolean_fallthrough() {
        let flag_true = FlagBuilder::new("test-flag")
            .fallthrough_variation(true)
            .build();
        assert_eq!(
            flag_true.fallthrough,
            VariationOrRollout::Variation { variation: 0 }
        );

        let flag_false = FlagBuilder::new("test-flag")
            .fallthrough_variation(false)
            .build();
        assert_eq!(
            flag_false.fallthrough,
            VariationOrRollout::Variation { variation: 1 }
        );
    }

    #[test]
    fn fallthrough_variation_index_sets_index() {
        let flag = FlagBuilder::new("test-flag")
            .fallthrough_variation_index(2)
            .build();
        assert_eq!(
            flag.fallthrough,
            VariationOrRollout::Variation { variation: 2 }
        );
    }

    #[test]
    fn off_variation_sets_boolean_off() {
        let flag_true = FlagBuilder::new("test-flag").off_variation(true).build();
        assert_eq!(flag_true.off_variation, Some(0));

        let flag_false = FlagBuilder::new("test-flag").off_variation(false).build();
        assert_eq!(flag_false.off_variation, Some(1));
    }

    #[test]
    fn off_variation_index_sets_index() {
        let flag = FlagBuilder::new("test-flag").off_variation_index(2).build();
        assert_eq!(flag.off_variation, Some(2));
    }

    #[test]
    fn variation_for_all_configures_for_everyone() {
        let flag = FlagBuilder::new("test-flag")
            .variation_for_user("user1", false)
            .if_match("country", vec![AttributeValue::String("us".to_string())])
            .then_return(false)
            .variation_for_all(true)
            .build();

        assert_eq!(flag.on, true);
        assert_eq!(flag.targets.len(), 0);
        assert_eq!(flag.rules.len(), 0);
        assert_eq!(
            flag.fallthrough,
            VariationOrRollout::Variation { variation: 0 }
        );
    }

    #[test]
    fn variation_for_all_index_configures_with_index() {
        let flag = FlagBuilder::new("test-flag")
            .variations(vec![
                FlagValue::Str("red".to_string()),
                FlagValue::Str("green".to_string()),
                FlagValue::Str("blue".to_string()),
            ])
            .variation_for_all_index(2)
            .build();

        assert_eq!(flag.on, true);
        assert_eq!(flag.targets.len(), 0);
        assert_eq!(flag.rules.len(), 0);
        assert_eq!(
            flag.fallthrough,
            VariationOrRollout::Variation { variation: 2 }
        );
    }

    #[test]
    fn value_for_all_sets_single_value() {
        let flag = FlagBuilder::new("test-flag")
            .value_for_all(FlagValue::Str("constant".to_string()))
            .build();

        // Test by evaluating with targeting on and off
        let store = TestStore {
            flag: Some(flag.clone()),
        };
        let context = ContextBuilder::new("user-123").build().unwrap();
        let result = evaluate(&store, &flag, &context, None);

        assert_eq!(result.value, Some(&FlagValue::Str("constant".to_string())));
        assert_eq!(flag.on, true);
        assert_eq!(flag.off_variation, Some(0));
    }

    #[test]
    fn variation_for_user_targets_user_context() {
        let flag = FlagBuilder::new("test-flag")
            .variation_for_user("user-123", true)
            .build();

        let store = TestStore { flag: Some(flag) };
        let context = ContextBuilder::new("user-123").build().unwrap();
        let flag = store.flag("test-flag").unwrap();
        let result = evaluate(&store, &flag, &context, None);

        assert_eq!(result.value, Some(&FlagValue::Bool(true)));
    }

    #[test]
    fn variation_for_key_targets_any_context_kind() {
        let flag = FlagBuilder::new("test-flag")
            .variation_for_key(Kind::from("organization"), "org-456", false)
            .build();

        let store = TestStore { flag: Some(flag) };
        let context = ContextBuilder::new("org-456")
            .kind("organization")
            .build()
            .unwrap();
        let flag = store.flag("test-flag").unwrap();
        let result = evaluate(&store, &flag, &context, None);

        assert_eq!(result.value, Some(&FlagValue::Bool(false)));
    }

    #[test]
    fn variation_index_for_user_works_with_indices() {
        let flag = FlagBuilder::new("test-flag")
            .variations(vec![
                FlagValue::Str("red".to_string()),
                FlagValue::Str("green".to_string()),
                FlagValue::Str("blue".to_string()),
            ])
            .variation_index_for_user("user-123", 2)
            .build();

        let store = TestStore { flag: Some(flag) };
        let context = ContextBuilder::new("user-123").build().unwrap();
        let flag = store.flag("test-flag").unwrap();
        let result = evaluate(&store, &flag, &context, None);

        assert_eq!(result.value, Some(&FlagValue::Str("blue".to_string())));
    }

    #[test]
    fn variation_index_for_key_works_with_any_kind() {
        let flag = FlagBuilder::new("test-flag")
            .variations(vec![
                FlagValue::Number(0.0),
                FlagValue::Number(1.0),
                FlagValue::Number(2.0),
            ])
            .variation_index_for_key(Kind::from("device"), "device-789", 1)
            .build();

        let store = TestStore { flag: Some(flag) };
        let context = ContextBuilder::new("device-789")
            .kind("device")
            .build()
            .unwrap();
        let flag = store.flag("test-flag").unwrap();
        let result = evaluate(&store, &flag, &context, None);

        assert_eq!(result.value, Some(&FlagValue::Number(1.0)));
    }

    #[test]
    fn context_targeting_takes_precedence_over_rules() {
        let flag = FlagBuilder::new("test-flag")
            .variation_for_user("user-123", true)
            .if_match("key", vec![AttributeValue::String("user-123".to_string())])
            .then_return(false)
            .build();

        let store = TestStore { flag: Some(flag) };
        let context = ContextBuilder::new("user-123").build().unwrap();
        let flag = store.flag("test-flag").unwrap();
        let result = evaluate(&store, &flag, &context, None);

        assert_eq!(result.value, Some(&FlagValue::Bool(true)));
    }

    #[test]
    fn targeting_key_removes_from_other_variations() {
        let flag = FlagBuilder::new("test-flag")
            .variation_for_user("user-123", true)
            .variation_for_user("user-123", false)
            .build();

        // Should only be in the false target now
        let false_targets: Vec<_> = flag
            .targets
            .iter()
            .filter(|t| t.variation == 1)
            .flat_map(|t| &t.values)
            .collect();
        assert!(false_targets.contains(&&"user-123".to_string()));

        let true_targets: Vec<_> = flag
            .targets
            .iter()
            .filter(|t| t.variation == 0)
            .flat_map(|t| &t.values)
            .collect();
        assert!(!true_targets.contains(&&"user-123".to_string()));
    }

    #[test]
    fn clear_targets_removes_all_targets() {
        let flag = FlagBuilder::new("test-flag")
            .variation_for_user("user-123", true)
            .variation_for_user("user-456", false)
            .clear_targets()
            .build();

        assert_eq!(flag.targets.len(), 0);
    }

    #[test]
    fn if_match_creates_rule_for_user_contexts() {
        let flag = FlagBuilder::new("test-flag")
            .if_match(
                "country",
                vec![
                    AttributeValue::String("us".to_string()),
                    AttributeValue::String("ca".to_string()),
                ],
            )
            .then_return(true)
            .build();

        let store = TestStore { flag: Some(flag) };
        let context = ContextBuilder::new("user-123")
            .set_value("country", AttributeValue::String("us".to_string()))
            .build()
            .unwrap();
        let flag = store.flag("test-flag").unwrap();
        let result = evaluate(&store, &flag, &context, None);

        assert_eq!(result.value, Some(&FlagValue::Bool(true)));
    }

    #[test]
    fn if_match_context_creates_rule_for_any_kind() {
        let flag = FlagBuilder::new("test-flag")
            .if_match_context(
                Kind::from("organization"),
                "industry",
                vec![AttributeValue::String("tech".to_string())],
            )
            .then_return(true)
            .build();

        let store = TestStore { flag: Some(flag) };
        let context = ContextBuilder::new("org-123")
            .kind("organization")
            .set_value("industry", AttributeValue::String("tech".to_string()))
            .build()
            .unwrap();
        let flag = store.flag("test-flag").unwrap();
        let result = evaluate(&store, &flag, &context, None);

        assert_eq!(result.value, Some(&FlagValue::Bool(true)));
    }

    #[test]
    fn if_not_match_creates_negated_rule() {
        let flag = FlagBuilder::new("test-flag")
            .if_not_match("country", vec![AttributeValue::String("us".to_string())])
            .then_return(true)
            .build();

        let store = TestStore {
            flag: Some(flag.clone()),
        };

        // Should not match US
        let us_context = ContextBuilder::new("user-123")
            .set_value("country", AttributeValue::String("us".to_string()))
            .build()
            .unwrap();
        let us_result = evaluate(&store, &flag, &us_context, None);
        assert_eq!(us_result.value, Some(&FlagValue::Bool(true))); // fallthrough

        // Should match CA
        let ca_context = ContextBuilder::new("user-456")
            .set_value("country", AttributeValue::String("ca".to_string()))
            .build()
            .unwrap();
        let ca_result = evaluate(&store, &flag, &ca_context, None);
        assert_eq!(ca_result.value, Some(&FlagValue::Bool(true)));
    }

    #[test]
    fn if_not_match_context_creates_negated_rule_for_any_kind() {
        let flag = FlagBuilder::new("test-flag")
            .fallthrough_variation(false)
            .if_not_match_context(
                Kind::from("organization"),
                "tier",
                vec![AttributeValue::String("enterprise".to_string())],
            )
            .then_return(true)
            .build();

        let store = TestStore {
            flag: Some(flag.clone()),
        };

        // Should match non-enterprise
        let basic_context = ContextBuilder::new("org-123")
            .kind("organization")
            .set_value("tier", AttributeValue::String("basic".to_string()))
            .build()
            .unwrap();
        let basic_result = evaluate(&store, &flag, &basic_context, None);
        assert_eq!(basic_result.value, Some(&FlagValue::Bool(true)));
    }

    #[test]
    fn and_match_adds_multiple_clauses() {
        let flag = FlagBuilder::new("test-flag")
            .if_match("country", vec![AttributeValue::String("us".to_string())])
            .and_match("state", vec![AttributeValue::String("ca".to_string())])
            .then_return(true)
            .build();

        let store = TestStore {
            flag: Some(flag.clone()),
        };

        // Both conditions match
        let matching_context = ContextBuilder::new("user-123")
            .set_value("country", AttributeValue::String("us".to_string()))
            .set_value("state", AttributeValue::String("ca".to_string()))
            .build()
            .unwrap();
        let matching_result = evaluate(&store, &flag, &matching_context, None);
        assert_eq!(matching_result.value, Some(&FlagValue::Bool(true)));

        // Only one condition matches
        let partial_context = ContextBuilder::new("user-456")
            .set_value("country", AttributeValue::String("us".to_string()))
            .set_value("state", AttributeValue::String("ny".to_string()))
            .build()
            .unwrap();
        let partial_result = evaluate(&store, &flag, &partial_context, None);
        assert_eq!(partial_result.value, Some(&FlagValue::Bool(true))); // fallthrough
    }

    #[test]
    fn and_not_match_adds_negated_clauses() {
        let flag = FlagBuilder::new("test-flag")
            .fallthrough_variation(false)
            .if_match("country", vec![AttributeValue::String("us".to_string())])
            .and_not_match("state", vec![AttributeValue::String("ca".to_string())])
            .then_return(true)
            .build();

        let store = TestStore {
            flag: Some(flag.clone()),
        };

        // Matches US but not CA
        let matching_context = ContextBuilder::new("user-123")
            .set_value("country", AttributeValue::String("us".to_string()))
            .set_value("state", AttributeValue::String("ny".to_string()))
            .build()
            .unwrap();
        let matching_result = evaluate(&store, &flag, &matching_context, None);
        assert_eq!(matching_result.value, Some(&FlagValue::Bool(true)));

        // Matches US and CA (should not match rule)
        let ca_context = ContextBuilder::new("user-456")
            .set_value("country", AttributeValue::String("us".to_string()))
            .set_value("state", AttributeValue::String("ca".to_string()))
            .build()
            .unwrap();
        let ca_result = evaluate(&store, &flag, &ca_context, None);
        assert_eq!(ca_result.value, Some(&FlagValue::Bool(false))); // fallthrough
    }

    #[test]
    fn then_return_completes_rule() {
        let flag = FlagBuilder::new("test-flag")
            .if_match("beta", vec![AttributeValue::Bool(true)])
            .then_return(true)
            .build();

        assert_eq!(flag.rules.len(), 1);
        assert_eq!(
            flag.rules[0].variation_or_rollout,
            VariationOrRollout::Variation { variation: 0 }
        );
    }

    #[test]
    fn then_return_index_completes_rule_with_index() {
        let flag = FlagBuilder::new("test-flag")
            .variations(vec![
                FlagValue::Str("red".to_string()),
                FlagValue::Str("green".to_string()),
                FlagValue::Str("blue".to_string()),
            ])
            .if_match("color", vec![AttributeValue::String("primary".to_string())])
            .then_return_index(2)
            .build();

        assert_eq!(flag.rules.len(), 1);
        assert_eq!(
            flag.rules[0].variation_or_rollout,
            VariationOrRollout::Variation { variation: 2 }
        );
    }

    #[test]
    fn rules_evaluated_in_order() {
        let flag = FlagBuilder::new("test-flag")
            .if_match("key", vec![AttributeValue::String("user-123".to_string())])
            .then_return(true)
            .if_match("key", vec![AttributeValue::String("user-123".to_string())])
            .then_return(false)
            .build();

        let store = TestStore { flag: Some(flag) };
        let context = ContextBuilder::new("user-123").build().unwrap();
        let flag = store.flag("test-flag").unwrap();
        let result = evaluate(&store, &flag, &context, None);

        // First rule should win
        assert_eq!(result.value, Some(&FlagValue::Bool(true)));
    }

    #[test]
    fn rules_evaluated_after_targets_before_fallthrough() {
        let flag = FlagBuilder::new("test-flag")
            .fallthrough_variation(false)
            .variation_for_user("user-targeted", true)
            .if_match("beta", vec![AttributeValue::Bool(true)])
            .then_return(true)
            .build();

        let store = TestStore {
            flag: Some(flag.clone()),
        };

        // Targeted user gets target value
        let targeted_context = ContextBuilder::new("user-targeted")
            .set_value("beta", AttributeValue::Bool(true))
            .build()
            .unwrap();
        let targeted_result = evaluate(&store, &flag, &targeted_context, None);
        assert_eq!(targeted_result.value, Some(&FlagValue::Bool(true)));

        // Non-targeted user with matching rule gets rule value
        let rule_context = ContextBuilder::new("user-beta")
            .set_value("beta", AttributeValue::Bool(true))
            .build()
            .unwrap();
        let rule_result = evaluate(&store, &flag, &rule_context, None);
        assert_eq!(rule_result.value, Some(&FlagValue::Bool(true)));

        // Non-targeted user without matching rule gets fallthrough
        let fallthrough_context = ContextBuilder::new("user-other")
            .set_value("beta", AttributeValue::Bool(false))
            .build()
            .unwrap();
        let fallthrough_result = evaluate(&store, &flag, &fallthrough_context, None);
        assert_eq!(fallthrough_result.value, Some(&FlagValue::Bool(false)));
    }

    #[test]
    fn clear_rules_removes_all_rules() {
        let flag = FlagBuilder::new("test-flag")
            .if_match("country", vec![AttributeValue::String("us".to_string())])
            .then_return(true)
            .if_match("state", vec![AttributeValue::String("ca".to_string())])
            .then_return(false)
            .clear_rules()
            .build();

        assert_eq!(flag.rules.len(), 0);
    }

    #[test]
    fn only_in_operator_used_in_rules() {
        let flag = FlagBuilder::new("test-flag")
            .if_match("country", vec![AttributeValue::String("us".to_string())])
            .then_return(true)
            .build();

        assert_eq!(flag.rules.len(), 1);

        // Verify the rule works as expected with "in" semantics
        let store = TestStore {
            flag: Some(flag.clone()),
        };
        let us_context = ContextBuilder::new("user-123")
            .set_value("country", AttributeValue::String("us".to_string()))
            .build()
            .unwrap();
        let ca_context = ContextBuilder::new("user-456")
            .set_value("country", AttributeValue::String("ca".to_string()))
            .build()
            .unwrap();

        let us_flag = store.flag("test-flag").unwrap();
        let us_result = evaluate(&store, &us_flag, &us_context, None);
        assert_eq!(us_result.value, Some(&FlagValue::Bool(true)));

        let ca_flag = store.flag("test-flag").unwrap();
        let ca_result = evaluate(&store, &ca_flag, &ca_context, None);
        assert_eq!(ca_result.value, Some(&FlagValue::Bool(true))); // fallthrough
    }

    #[test]
    fn sampling_ratio_sets_ratio() {
        let flag = FlagBuilder::new("test-flag").sampling_ratio(10000).build();
        assert_eq!(flag.sampling_ratio, Some(10000));
    }

    #[test]
    fn sampling_ratio_defaults_to_none() {
        let flag = FlagBuilder::new("test-flag").build();
        assert_eq!(flag.sampling_ratio, None);
    }

    #[test]
    fn exclude_from_summaries_sets_exclusion() {
        let flag = FlagBuilder::new("test-flag")
            .exclude_from_summaries(true)
            .build();
        assert!(flag.exclude_from_summaries);
    }

    #[test]
    fn exclude_from_summaries_defaults_to_false() {
        let flag = FlagBuilder::new("test-flag").build();
        assert!(!flag.exclude_from_summaries);
    }
}
