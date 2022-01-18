use crate::flag::Flag;
use crate::segment::Segment;

/// Store is an interface for a data store that holds feature flags and related data received by
/// the SDK.
///
/// Ordinarily, the only implementations of this interface are the default in-memory
/// implementation, which holds references to actual SDK data model objects.
pub trait Store {
    /// Retrieve the flag with key `flag_key`.
    fn flag(&self, flag_key: &str) -> Option<&Flag>;

    /// Retrieve the segment with key `segment_key`.
    fn segment(&self, segment_key: &str) -> Option<&Segment>;
}
