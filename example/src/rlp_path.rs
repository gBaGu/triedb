use std::iter::once;

use rlp::Rlp;


#[derive(Clone, Debug, Default)]
pub struct RlpPath {
    path: Vec<usize>,
}

impl RlpPath {
    pub fn new(other: &RlpPath, i: usize) -> Self {
        Self {
            path: other.path.iter().copied().chain(once(i)).collect()
        }
    }

    pub fn get_subslice<'a>(&self, input: &'a[u8]) -> &'a[u8] {
        let mut tmp = input;
        for i in &self.path {
            tmp = Rlp::new(tmp).at(*i).unwrap().data().unwrap()
        }
        tmp
    }
}