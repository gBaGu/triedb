use rlp::Rlp;


pub trait RlpPath {
    fn get_bytes<'a>(&self, input: &'a[u8]) -> &'a[u8];
}


#[derive(Copy, Clone, Debug)]
pub struct Empty {}

impl RlpPath for Empty {
    fn get_bytes<'a>(&self, input: &'a[u8]) -> &'a[u8] {
        input
    }
}


#[derive(Copy, Clone, Debug)]
pub struct ListNthValue<Parent: RlpPath> {
    pub parent: Parent,
    pub i: usize,
}

impl<Parent: RlpPath> ListNthValue<Parent> {
    pub fn new(parent: Parent, i: usize) -> Self {
        Self { parent, i }
    }
}

impl<Parent: RlpPath> RlpPath for ListNthValue<Parent> {
    fn get_bytes<'a>(&self, input: &'a[u8]) -> &'a[u8] {
        Rlp::new(self.parent.get_bytes(input)).at(self.i).unwrap().data().unwrap()
    }
}