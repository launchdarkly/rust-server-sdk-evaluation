# Change log

All notable changes to the project will be documented in this file. This project adheres to [Semantic Versioning](http://semver.org).

## [2.0.1](https://github.com/launchdarkly/rust-server-sdk-evaluation/compare/2.0.0...2.0.1) (2025-01-28)


### Bug Fixes

* Bump MSRV to 1.74.0 ([#26](https://github.com/launchdarkly/rust-server-sdk-evaluation/issues/26)) ([44e5650](https://github.com/launchdarkly/rust-server-sdk-evaluation/commit/44e5650a42f51f24df99c4352989fc743fe2b2ce))
* Update base16ct to v0.2.0 ([aac23e0](https://github.com/launchdarkly/rust-server-sdk-evaluation/commit/aac23e085469d5d07f1d8601da31c0c2cd5fe33c))
* Update itertools to v0.14.0 ([aac23e0](https://github.com/launchdarkly/rust-server-sdk-evaluation/commit/aac23e085469d5d07f1d8601da31c0c2cd5fe33c))
* Update serde_with to v3.12.0 ([aac23e0](https://github.com/launchdarkly/rust-server-sdk-evaluation/commit/aac23e085469d5d07f1d8601da31c0c2cd5fe33c))

## [2.0.0](https://github.com/launchdarkly/rust-server-sdk-evaluation/compare/1.2.0...2.0.0) (2024-07-10)


### ⚠ BREAKING CHANGES

* Add exclude from summary field to flag ([#18](https://github.com/launchdarkly/rust-server-sdk-evaluation/issues/18))
* Add migration and sampling ratio fields ([#17](https://github.com/launchdarkly/rust-server-sdk-evaluation/issues/17))

### Features

* Add exclude from summary field to flag ([#18](https://github.com/launchdarkly/rust-server-sdk-evaluation/issues/18)) ([ca935a3](https://github.com/launchdarkly/rust-server-sdk-evaluation/commit/ca935a31f9caca6001bffeb7bfd6adfafff7b755))
* Add method to strip anonymous contexts ([#21](https://github.com/launchdarkly/rust-server-sdk-evaluation/issues/21)) ([f76faa6](https://github.com/launchdarkly/rust-server-sdk-evaluation/commit/f76faa685c7bc3494e0fcbeff04a8641277efc91))
* Add migration and sampling ratio fields ([#17](https://github.com/launchdarkly/rust-server-sdk-evaluation/issues/17)) ([5e4e969](https://github.com/launchdarkly/rust-server-sdk-evaluation/commit/5e4e96940223bb3cc9f50b3ca45c5f4fa253c2db))
* Add serialize trait to Detail ([#16](https://github.com/launchdarkly/rust-server-sdk-evaluation/issues/16)) ([683231e](https://github.com/launchdarkly/rust-server-sdk-evaluation/commit/683231ec193897e69773ccd6aaf780ea0805cbeb))


### Bug Fixes

* Bump msrv to 1.67.1 ([#22](https://github.com/launchdarkly/rust-server-sdk-evaluation/issues/22)) ([4b02096](https://github.com/launchdarkly/rust-server-sdk-evaluation/commit/4b02096ea3480b4ee5604e77f9838c4ae45d982d))

## [1.2.0](https://github.com/launchdarkly/rust-server-sdk-evaluation/compare/1.1.1...1.2.0) (2024-03-14)


### Features

* Support redacting all attributes from anonymous contexts ([#14](https://github.com/launchdarkly/rust-server-sdk-evaluation/issues/14)) ([76d5e52](https://github.com/launchdarkly/rust-server-sdk-evaluation/commit/76d5e5290d3a33c8bc939f3d66f7f3d1e61aaffc))

## [1.1.1] - 2023-08-07
### Fixed:
- Fixed an issue with evaluation of segments which included/excluded users when that user was part of a multi-context.

## [1.1.0] - 2023-05-05
### Changed:
- Updated MSRV from 1.60.0 to 1.64.0

### Fixed:
- Fix invalid encoding for fully qualified context key generation.

## [1.0.0] - 2022-12-06
This release of the evaluation engine corresponds to the upcoming v1.0.0 release of the LaunchDarkly server-side Rust SDK (launchdarkly-server-sdk), and is not compatible with earlier SDK versions.

### Added:
- Added the types `Context`, `Kind`, `ContextBuilder`, `MultiContextBuilder`, `Reference`, and supporting types. `Context` defines the new context-based evaluation model. `Context` replace `User`, which was removed.
- Added: `urlencoding` @ `2.1.0`
- Added: `maplit` @ `1.0.1`
- Added: `itertools` @ `0.10.3`
- Added: `serde_with` @ `2.1.0`


### Changed:
- The [MSRV](https://rust-lang.github.io/rfcs/2495-min-rust-version.html) is now 1.60.0
- Updated: `chrono` from `0.4.15` to `0.4.23`; only enable the `"std"` feature
- Updated: `semver` from `0.10.0` to `1.0.14`
- `evaluate` now takes a `Context` instead of a `User`
- User keys could previously be empty strings. With contexts, the key cannot be empty. 
- The "secondary" meta-attribute which affected percentage rollouts has been removed. If you set an attribute with that name in a context, it will be a custom attribute like any other.
- For backwards-compatibility, it is possible to enable "secondary" evaluation logic within the evaluation engine by enabling the `secondary_key_bucketing` flag. This will only affect contexts that were created via deserialization, since it is not possible to set "secondary" via a builder method. 

### Removed:
- `User`, `UserBuilder`, `UserAttributes`. See `Context`, `ContextBuilder` and `MultiContextBuilder` instead.

## [1.0.0-beta.5] - 2022-04-04
### Changed
- Update to edition 2021.
- Change store signature to support persistent stores.

### Added
- Support flag serialization of old and current schema.
- Add versioned trait (implemented by flag and segment).

## [1.0.0-beta.4] - 2022-03-07
### Changed
- Bump sha1 and test\_case dependencies.

### Fixed
- When serializing a user, custom attributes were incorrectly being flattened
  in the JSON output. We now correctly encode them under a `custom` key.

## [1.0.0-beta.3] - 2022-02-16
### Added
- Add support for globally and user specific private attributes.

### Changed
- If we receive a negative variation index, or an unsupported operation, the
  SDK should not fail to parse the payload.

## [1.0.0-beta.2] - 2022-01-21
### Changed
- Modified the `try_map` function to accept a default value when returning a new Detail instance.

## [1.0.0-beta.1] - 2022-01-19
Initial release of flag evaluation support code that will be used with the LaunchDarkly Server-Side SDK for Rust.
