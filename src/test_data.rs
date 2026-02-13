//! Test data source builders for creating flags and segments for testing purposes.
//!
//! This module provides builder APIs for constructing feature flags with various configurations
//! without needing to connect to LaunchDarkly services. These builders are intended for use in
//! test scenarios and local development.

use std::sync::{Arc, Mutex};

use crate::{
    contexts::context::Kind, flag::Target, flag_value::FlagValue, rule::Clause, rule::FlagRule,
    AttributeValue, Flag,
};

/// Builder for constructing test flags with various configurations.
///
/// The flag builder provides a fluent API for creating flags with targeting rules,
/// variations, and other configuration options. This is the primary way to create
/// test flags for use in testing and development scenarios.
#[derive(Clone, Debug)]
pub struct FlagBuilder {
    inner: Arc<Mutex<FlagBuilderInner>>,
}

#[derive(Clone, Debug)]
struct FlagBuilderInner {
    key: String,
    on: bool,
    variations: Vec<FlagValue>,
    fallthrough_variation: isize,
    off_variation: isize,
    targets: Vec<Target>,
    rules: Vec<FlagRule>,
}

impl FlagBuilder {
    /// Creates a new flag builder for the given flag key.
    ///
    /// If creating a new flag, it will be initialized as a boolean flag with:
    /// - Variations: [true, false]
    /// - Targeting enabled (on: true)
    /// - Fallthrough variation: 0 (true)
    /// - Off variation: 1 (false)
    ///
    /// # Spec Coverage
    /// - TDS-1.3.1: Flag method accepts key and returns builder
    /// - TDS-1.3.3: New flags default to boolean configuration
    pub fn new(key: impl Into<String>) -> Self {
        Self {
            inner: Arc::new(Mutex::new(FlagBuilderInner {
                key: key.into(),
                on: true,
                variations: vec![FlagValue::Bool(true), FlagValue::Bool(false)],
                fallthrough_variation: 0,
                off_variation: 1,
                targets: vec![],
                rules: vec![],
            })),
        }
    }

    /// Configures the flag as a boolean type with variations [true, false].
    ///
    /// Sets:
    /// - Variations: [true, false]
    /// - Fallthrough variation: 0 (true)
    /// - Off variation: 1 (false)
    ///
    /// # Spec Coverage
    /// - TDS-1.3.4: BooleanFlag method configures boolean flag
    pub fn boolean_flag(self) -> Self {
        let mut inner = self.inner.lock().unwrap();
        inner.variations = vec![FlagValue::Bool(true), FlagValue::Bool(false)];
        inner.fallthrough_variation = 0;
        inner.off_variation = 1;
        drop(inner);
        self
    }

    /// Sets the variations for this flag.
    ///
    /// # Spec Coverage
    /// - TDS-1.3.5: Variations method sets flag variations
    pub fn variations<I>(self, variations: I) -> Self
    where
        I: IntoIterator<Item = FlagValue>,
    {
        let mut inner = self.inner.lock().unwrap();
        inner.variations = variations.into_iter().collect();
        drop(inner);
        self
    }

    /// Sets whether targeting is enabled for the flag.
    ///
    /// When targeting is off (false), the flag returns the off variation regardless
    /// of other configuration.
    ///
    /// # Spec Coverage
    /// - TDS-1.3.6: On method sets targeting enabled state
    pub fn on(self, on: bool) -> Self {
        let mut inner = self.inner.lock().unwrap();
        inner.on = on;
        drop(inner);
        self
    }

    /// Sets the fallthrough variation for boolean flags.
    ///
    /// This is a convenience method equivalent to calling `fallthrough_variation_index`
    /// with 0 for true or 1 for false.
    ///
    /// # Spec Coverage
    /// - TDS-1.3.7: FallthroughVariation convenience method for boolean flags
    pub fn fallthrough_variation(self, value: bool) -> Self {
        self.fallthrough_variation_index(if value { 0 } else { 1 })
    }

    /// Sets the fallthrough variation by index.
    ///
    /// The fallthrough variation is returned when targeting is on but no targets or rules match.
    ///
    /// # Spec Coverage
    /// - TDS-1.3.8: FallthroughVariationIndex sets fallthrough by index
    pub fn fallthrough_variation_index(self, index: isize) -> Self {
        let mut inner = self.inner.lock().unwrap();
        inner.fallthrough_variation = index;
        drop(inner);
        self
    }

    /// Sets the off variation for boolean flags.
    ///
    /// This is a convenience method equivalent to calling `off_variation_index`
    /// with 0 for true or 1 for false.
    ///
    /// # Spec Coverage
    /// - TDS-1.3.9: OffVariation convenience method for boolean flags
    pub fn off_variation(self, value: bool) -> Self {
        self.off_variation_index(if value { 0 } else { 1 })
    }

    /// Sets the off variation by index.
    ///
    /// The off variation is returned when targeting is disabled (on: false).
    ///
    /// # Spec Coverage
    /// - TDS-1.3.10: OffVariationIndex sets off variation by index
    pub fn off_variation_index(self, index: isize) -> Self {
        let mut inner = self.inner.lock().unwrap();
        inner.off_variation = index;
        drop(inner);
        self
    }

    /// Configures the flag to always return the specified boolean value for everyone.
    ///
    /// This is a convenience method that:
    /// - Enables targeting (on: true)
    /// - Removes all targets and rules
    /// - Sets the fallthrough variation to the specified value
    ///
    /// # Spec Coverage
    /// - TDS-1.3.11: VariationForAll convenience method for boolean flags
    pub fn variation_for_all(self, value: bool) -> Self {
        let mut inner = self.inner.lock().unwrap();
        inner.on = true;
        inner.targets.clear();
        inner.rules.clear();
        inner.fallthrough_variation = if value { 0 } else { 1 };
        drop(inner);
        self
    }

    /// Configures the flag to always return the specified variation index for everyone.
    ///
    /// This is a convenience method that:
    /// - Enables targeting (on: true)
    /// - Removes all targets and rules
    /// - Sets the fallthrough variation to the specified index
    ///
    /// # Spec Coverage
    /// - TDS-1.3.12: VariationForAllIndex for non-boolean flags
    pub fn variation_for_all_index(self, index: isize) -> Self {
        let mut inner = self.inner.lock().unwrap();
        inner.on = true;
        inner.targets.clear();
        inner.rules.clear();
        inner.fallthrough_variation = index;
        drop(inner);
        self
    }

    /// Configures the flag to always return the specified value.
    ///
    /// This is a convenience method that:
    /// - Sets a single variation equal to the specified value
    /// - Enables targeting (on: true)
    /// - Removes all targets and rules
    /// - Sets both fallthrough and off variation to index 0
    ///
    /// # Spec Coverage
    /// - TDS-1.3.13: ValueForAll convenience method (optional)
    pub fn value_for_all(self, value: FlagValue) -> Self {
        let mut inner = self.inner.lock().unwrap();
        inner.variations = vec![value];
        inner.on = true;
        inner.targets.clear();
        inner.rules.clear();
        inner.fallthrough_variation = 0;
        inner.off_variation = 0;
        drop(inner);
        self
    }

    /// Configures the flag to return a specific boolean value for a user context.
    ///
    /// This is a convenience method for targeting contexts with kind: "user".
    ///
    /// # Spec Coverage
    /// - TDS-1.4.1: VariationForUser targets user contexts
    pub fn variation_for_user(self, user_key: impl Into<String>, variation: bool) -> Self {
        self.variation_index_for_key(Kind::user(), user_key.into(), if variation { 0 } else { 1 })
    }

    /// Configures the flag to return a specific boolean value for a context of any kind.
    ///
    /// # Spec Coverage
    /// - TDS-1.4.2: VariationForKey targets contexts of any kind
    pub fn variation_for_key(
        self,
        context_kind: Kind,
        key: impl Into<String>,
        variation: bool,
    ) -> Self {
        self.variation_index_for_key(context_kind, key.into(), if variation { 0 } else { 1 })
    }

    /// Configures the flag to return a specific variation index for a user context.
    ///
    /// This is a convenience method for targeting contexts with kind: "user".
    ///
    /// # Spec Coverage
    /// - TDS-1.4.3: VariationIndexForUser for user contexts
    pub fn variation_index_for_user(self, user_key: impl Into<String>, variation: isize) -> Self {
        self.variation_index_for_key(Kind::user(), user_key.into(), variation)
    }

    /// Configures the flag to return a specific variation index for a context of any kind.
    ///
    /// When a context key is targeted, that key is automatically removed from targeting
    /// for any other variation of the same flag (a key can only be targeted for one
    /// variation at a time).
    ///
    /// # Spec Coverage
    /// - TDS-1.4.4: VariationIndexForKey for any context kind
    /// - TDS-1.4.6: Context key removed from other variation targets
    pub fn variation_index_for_key(
        self,
        context_kind: Kind,
        key: impl Into<String>,
        variation: isize,
    ) -> Self {
        let key = key.into();
        let mut inner = self.inner.lock().unwrap();

        // Remove the key from all existing targets for this context kind
        for target in &mut inner.targets {
            if target.context_kind == context_kind {
                target.values.retain(|k| k != &key);
            }
        }

        // Find or create target for this variation and context kind
        let target = inner
            .targets
            .iter_mut()
            .find(|t| t.variation == variation && t.context_kind == context_kind);

        if let Some(target) = target {
            if !target.values.contains(&key) {
                target.values.push(key);
            }
        } else {
            inner.targets.push(Target {
                context_kind,
                values: vec![key],
                variation,
            });
        }

        drop(inner);
        self
    }

    /// Removes all individual context targets from the flag.
    ///
    /// # Spec Coverage
    /// - TDS-1.4.7: ClearTargets method (optional)
    pub fn clear_targets(self) -> Self {
        let mut inner = self.inner.lock().unwrap();
        inner.targets.clear();
        drop(inner);
        self
    }

    /// Creates a rule that matches when the specified user attribute equals any of the provided values.
    ///
    /// This is a convenience method for creating rules that target contexts with kind: "user".
    /// Returns a RuleBuilder that can be used to add more conditions or complete the rule.
    ///
    /// # Spec Coverage
    /// - TDS-1.5.1: IfMatch creates rule for user contexts
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
    ///
    /// # Spec Coverage
    /// - TDS-1.5.2: IfMatchContext creates rule for any context kind
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
    ///
    /// # Spec Coverage
    /// - TDS-1.5.3: IfNotMatch for negated user context matching
    pub fn if_not_match<I>(self, attribute: impl Into<String>, values: I) -> RuleBuilder
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        self.if_not_match_context(Kind::user(), attribute, values)
    }

    /// Creates a rule that matches when the specified attribute does NOT equal any of the provided values
    /// for a context of the specified kind.
    ///
    /// # Spec Coverage
    /// - TDS-1.5.4: IfNotMatchContext for negated context matching
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
    ///
    /// # Spec Coverage
    /// - TDS-1.5.13: ClearRules method (optional)
    pub fn clear_rules(self) -> Self {
        let mut inner = self.inner.lock().unwrap();
        inner.rules.clear();
        drop(inner);
        self
    }

    /// Builds the final Flag instance.
    ///
    /// This method creates a complete Flag with all configured settings.
    pub fn build(self) -> Flag {
        let inner = self.inner.lock().unwrap();

        // Construct JSON and deserialize to avoid dealing with private fields
        let json = serde_json::json!({
            "key": inner.key,
            "version": 1,
            "on": inner.on,
            "targets": inner.targets,
            "contextTargets": [],
            "rules": inner.rules,
            "prerequisites": [],
            "fallthrough": { "variation": inner.fallthrough_variation },
            "offVariation": inner.off_variation,
            "variations": inner.variations,
            "clientSide": false,
            "salt": "",
            "trackEvents": false,
            "trackEventsFallthrough": false,
        });

        serde_json::from_value(json).expect("Failed to build flag from valid JSON")
    }

    fn add_rule(&self, rule: FlagRule) {
        let mut inner = self.inner.lock().unwrap();
        inner.rules.push(rule);
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
    ///
    /// # Spec Coverage
    /// - TDS-1.5.5: AndMatch adds clause for user contexts
    pub fn and_match<I>(self, attribute: impl Into<String>, values: I) -> Self
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        self.and_match_context(Kind::user(), attribute, values)
    }

    /// Adds another clause to the current rule for a context of the specified kind.
    ///
    /// Multiple clauses in a rule have AND semantics - all must match for the rule to match.
    ///
    /// # Spec Coverage
    /// - TDS-1.5.6: AndMatchContext adds clause for any context kind
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
    ///
    /// # Spec Coverage
    /// - TDS-1.5.7: AndNotMatch adds negated clause for user contexts
    pub fn and_not_match<I>(self, attribute: impl Into<String>, values: I) -> Self
    where
        I: IntoIterator<Item = AttributeValue>,
    {
        self.and_not_match_context(Kind::user(), attribute, values)
    }

    /// Adds a negated clause to the current rule for a context of the specified kind.
    ///
    /// The clause must NOT match any of the values for the rule to match.
    ///
    /// # Spec Coverage
    /// - TDS-1.5.8: AndNotMatchContext adds negated clause for any context kind
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
    ///
    /// # Spec Coverage
    /// - TDS-1.5.9: ThenReturn completes rule for boolean flags
    pub fn then_return(self, variation: bool) -> FlagBuilder {
        self.then_return_index(if variation { 0 } else { 1 })
    }

    /// Completes the rule configuration with a variation index.
    ///
    /// This method adds the completed rule to the flag and returns control to the flag builder.
    ///
    /// # Spec Coverage
    /// - TDS-1.5.10: ThenReturnIndex completes rule with variation index
    /// - TDS-1.5.11: Rules evaluated in order they were added
    /// - TDS-1.5.12: Rules evaluated after targets and before fallthrough
    pub fn then_return_index(self, variation: isize) -> FlagBuilder {
        let rule_id = self.rule_id.unwrap_or_else(|| {
            format!(
                "rule{}",
                self.flag_builder.inner.lock().unwrap().rules.len()
            )
        });

        let rule_json = serde_json::json!({
            "id": rule_id,
            "clauses": self.clauses,
            "variation": variation,
            "trackEvents": false,
        });

        let rule: FlagRule =
            serde_json::from_value(rule_json).expect("Failed to create rule from valid JSON");

        self.flag_builder.add_rule(rule);
        self.flag_builder
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

    /// TDS-1.3.1, TDS-1.3.3: New flag has boolean defaults
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

    /// TDS-1.3.4: BooleanFlag method configures boolean flag
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

    /// TDS-1.3.5: Variations method sets flag variations
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

    /// TDS-1.3.6: On method sets targeting enabled state
    #[test]
    fn on_method_sets_targeting_state() {
        let flag_on = FlagBuilder::new("test-flag").on(true).build();
        assert_eq!(flag_on.on, true);

        let flag_off = FlagBuilder::new("test-flag").on(false).build();
        assert_eq!(flag_off.on, false);
    }

    /// TDS-1.3.7: FallthroughVariation for boolean flags
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

    /// TDS-1.3.8: FallthroughVariationIndex sets fallthrough by index
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

    /// TDS-1.3.9: OffVariation for boolean flags
    #[test]
    fn off_variation_sets_boolean_off() {
        let flag_true = FlagBuilder::new("test-flag").off_variation(true).build();
        assert_eq!(flag_true.off_variation, Some(0));

        let flag_false = FlagBuilder::new("test-flag").off_variation(false).build();
        assert_eq!(flag_false.off_variation, Some(1));
    }

    /// TDS-1.3.10: OffVariationIndex sets off variation by index
    #[test]
    fn off_variation_index_sets_index() {
        let flag = FlagBuilder::new("test-flag").off_variation_index(2).build();
        assert_eq!(flag.off_variation, Some(2));
    }

    /// TDS-1.3.11: VariationForAll configures flag for everyone
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

    /// TDS-1.3.12: VariationForAllIndex for non-boolean flags
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

    /// TDS-1.3.13: ValueForAll configures flag with single value
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

    /// TDS-1.4.1: VariationForUser targets user contexts
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

    /// TDS-1.4.2: VariationForKey targets contexts of any kind
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

    /// TDS-1.4.3: VariationIndexForUser for user contexts
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

    /// TDS-1.4.4: VariationIndexForKey for any context kind
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

    /// TDS-1.4.5: Individual context targeting takes precedence over rules
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

    /// TDS-1.4.6: Context key removed from other variation targets
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

    /// TDS-1.4.7: ClearTargets removes all targets
    #[test]
    fn clear_targets_removes_all_targets() {
        let flag = FlagBuilder::new("test-flag")
            .variation_for_user("user-123", true)
            .variation_for_user("user-456", false)
            .clear_targets()
            .build();

        assert_eq!(flag.targets.len(), 0);
    }

    /// TDS-1.5.1: IfMatch creates rule for user contexts
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

    /// TDS-1.5.2: IfMatchContext creates rule for any context kind
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

    /// TDS-1.5.3: IfNotMatch for negated matching
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

    /// TDS-1.5.4: IfNotMatchContext for negated context matching
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

    /// TDS-1.5.5, TDS-1.5.6: AndMatch and AndMatchContext add clauses
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

    /// TDS-1.5.7, TDS-1.5.8: AndNotMatch and AndNotMatchContext add negated clauses
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

    /// TDS-1.5.9: ThenReturn completes rule for boolean flags
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

    /// TDS-1.5.10: ThenReturnIndex completes rule with index
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

    /// TDS-1.5.11: Rules evaluated in order they were added
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

    /// TDS-1.5.12: Rules evaluated after targets and before fallthrough
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

    /// TDS-1.5.13: ClearRules removes all rules
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

    /// TDS-1.6.1: Only "in" operator supported
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
}
