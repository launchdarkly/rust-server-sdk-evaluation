use crate::flag::Flag;
use crate::segment::Segment;

pub trait Store {
    fn flag(&self, flag_key: &str) -> Option<&Flag>;

    fn segment(&self, segment_key: &str) -> Option<&Segment>;
}
