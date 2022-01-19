# Contributing to the LaunchDarky Rust SDK Evaluation Engine

LaunchDarkly has published an [SDK contributor's guide](https://docs.launchdarkly.com/sdk/concepts/contributors-guide) that provides a detailed explanation of how our SDKs work. See below for additional information on how to contribute to this SDK.

## Submitting bug reports and feature requests

The LaunchDarkly SDK team monitors the [issue tracker](https://github.com/launchdarkly/rust-server-sdk-evaluation/issues) in this repository. Bug reports and feature requests specific to this SDK should be filed in this issue tracker. The SDK team will respond to all newly filed issues within two business days. For issues or requests that are more generally related to the LaunchDarkly Rust SDK, rather than specifically for the code in this repository, please use the [`rust-server-sdk`](https://github.com/launchdarkly/rust-server-sdk) repository.

## Submitting pull requests

We encourage pull requests and other contributions from the community. Before submitting pull requests, ensure that all temporary or unintended code is removed. Don't worry about adding reviewers to the pull request; the LaunchDarkly SDK team will add themselves. The SDK team will acknowledge all pull requests within two business days.

## Build instructions

### Prerequisites

This project should be built against Rust 2018 edition.

### Building

To build the project without running any tests:
```
cargo build
```

If you wish to clean your working directory between builds, you can clean it by running:
```
cargo clean
```

To run the linter:
```
cargo clippy --all-features -- -D warnings
```

### Testing

To build and run all unit tests:
```
cargo test
```

## Coding best practices

### Test coverage

You can view the latest code coverage report in CircleCI, as `coverage/index.html` in the artifacts. You can also generate this information locally with `./coverage.sh`.
