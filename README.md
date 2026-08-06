# LaunchDarkly Rust SDK Evaluation Engine

[![Run CI](https://github.com/launchdarkly/rust-server-sdk-evaluation/actions/workflows/ci.yml/badge.svg)](https://github.com/launchdarkly/rust-server-sdk-evaluation/actions/workflows/ci.yml)

## LaunchDarkly overview

[LaunchDarkly](https://www.launchdarkly.com) is a feature management platform that serves trillions of feature flags daily to help teams build better software, faster. [Get started](https://docs.launchdarkly.com/home/getting-started) using LaunchDarkly today!

[![Twitter Follow](https://img.shields.io/twitter/follow/launchdarkly.svg?style=social&label=Follow&maxAge=2592000)](https://twitter.com/intent/follow?screen_name=launchdarkly)

## Getting started

This repository contains the internal feature flag evaluation logic and data model used by the [LaunchDarkly Rust SDK](https://github.com/launchdarkly/rust-server-sdk). It is packaged separately because it is also used by internal LaunchDarkly components. Applications using the LaunchDarkly Rust SDK should not need to reference this package directly.

## Cargo features

| Feature | Default | Description |
| --- | --- | --- |
| `float-roundtrip` | yes | Enables `serde_json`'s `float_roundtrip` feature so that fractional JSON numbers deserialize to the same `f64` that Go's `encoding/json` produces. |
| `secondary_key_bucketing` | no | Uses a secondary key present in context data when computing the context's bucket. Intended for backwards compatibility in non-SDK applications; secondary keys cannot be set using the context builders. |

### Disabling `float-roundtrip`

`float-roundtrip` is enabled by default because it is what keeps numeric flag values and numeric context attributes consistent with the other LaunchDarkly SDKs. Without it, `serde_json` uses a faster best-effort float parser that can land one unit in the last place away from the correctly-rounded value, so a numeric evaluation could in principle differ from what another SDK computes for the same flag.

Disable it if you would rather have the faster parser and do not depend on that cross-SDK consistency:

```toml
launchdarkly-server-sdk-evaluation = { version = "2", default-features = false }
```

Note that Cargo feature unification is additive and applies to the whole build, so `float_roundtrip` remains enabled if any other crate in your dependency graph asks for it.

## Learn more

Read our [documentation](http://docs.launchdarkly.com) for in-depth instructions on configuring and using LaunchDarkly. You can also head straight to the [complete reference guide for the Rust SDK](https://docs.launchdarkly.com/sdk/server-side/rust), or the [generated API documentation](https://docs.rs/launchdarkly-server-sdk-evaluation) for this project.

## Minimum Supported Rust Version

This project aims to maintain compatibility with the latest stable release of Rust in addition to the two prior minor releases.

Version updates may occur more frequently than the policy guideline states if external forces require it. For example, a CVE in a downstream dependency requiring an MSRV bump would be considered an acceptable reason to violate the six month guideline.

## Contributing

We encourage pull requests and other contributions from the community. Check out our [contributing guidelines](CONTRIBUTING.md) for instructions on how to contribute to this SDK.

## About LaunchDarkly

* LaunchDarkly is a continuous delivery platform that provides feature flags as a service and allows developers to iterate quickly and safely. We allow you to easily flag your features and manage them from the LaunchDarkly dashboard.  With LaunchDarkly, you can:
    * Roll out a new feature to a subset of your users (like a group of users who opt-in to a beta tester group), gathering feedback and bug reports from real-world use cases.
    * Gradually roll out a feature to an increasing percentage of users, and track the effect that the feature has on key metrics (for instance, how likely is a user to complete a purchase if they have feature A versus feature B?).
    * Turn off a feature that you realize is causing performance problems in production, without needing to re-deploy, or even restart the application with a changed configuration file.
    * Grant access to certain features based on user attributes, like payment plan (eg: users on the ‘gold’ plan get access to more features than users in the ‘silver’ plan). Disable parts of your application to facilitate maintenance, without taking everything offline.
* LaunchDarkly provides feature flag SDKs for a wide variety of languages and technologies. Read [our documentation](https://docs.launchdarkly.com/docs) for a complete list.
* Explore LaunchDarkly
    * [launchdarkly.com](https://www.launchdarkly.com/ "LaunchDarkly Main Website") for more information
    * [docs.launchdarkly.com](https://docs.launchdarkly.com/  "LaunchDarkly Documentation") for our documentation and SDK reference guides
    * [apidocs.launchdarkly.com](https://apidocs.launchdarkly.com/  "LaunchDarkly API Documentation") for our API documentation
    * [launchdarkly.com/blog](https://launchdarkly.com/blog/  "LaunchDarkly Blog Documentation") for the latest product updates
