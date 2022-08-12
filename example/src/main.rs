use std::fmt;
use std::ops::Deref;
use std::sync::Arc;
use primitive_types::H256;
use rlp::{Prototype, Rlp, RlpStream};
use serde::{Deserialize, Serialize};
use ouroboros::self_referencing;
use rocksdb;

mod rlp_path;
use rlp_path::{ListNthValue, RlpPath};


const fn empty_nodes() -> [MerkleValue; 16] {
    [
        MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty, MerkleValue::Empty,
    ]
}


trait Database {
    fn get(&self, key: H256) -> DbValueRef;
}

#[derive(Default)]
struct MyStruct {
    pub data: dashmap::DashMap<H256, Vec<u8>>
}

impl Database for MyStruct {
    fn get(&self, key: H256) -> DbValueRef {
        DbValueRef::Dashmap(Arc::new(self.data
            .get(&key)
            .unwrap_or_else(|| panic!("Value for {} not found in database", key))))
    }
}

#[derive(Clone)]
pub enum DbValueRef<'a> {
    Plain(&'a[u8]),
    Dashmap(Arc<dashmap::mapref::one::Ref<'a, H256, Vec<u8>>>),
    Rocks(Arc<rocksdb::DBPinnableSlice<'a>>),
}

impl<'a> fmt::Debug for DbValueRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.deref().fmt(f)
    }
}

impl<'a> Deref for DbValueRef<'a> {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        match self {
            Self::Plain(bytes) => *bytes,
            Self::Dashmap(r) => r.value(),
            Self::Rocks(slice) => slice.deref(),
        }
    }
}

#[self_referencing]
pub struct RlpWithDbValueRef<'a, P> {
    value_ref: DbValueRef<'a>,
    rlp_path: P,
    #[borrows(value_ref)]
    #[covariant]
    rlp: Rlp<'this>,
}

impl<'a, P: RlpPath + Copy> RlpWithDbValueRef<'a, P> {
    fn get_ref(&self) -> DbValueRef<'a> {
        self.borrow_value_ref().clone()
    }

    fn get_path(&self) -> P {
        *self.borrow_rlp_path()
    }
}
//
// impl<'a> Deref for RlpWithDbValueRef<'a> {
//     type Target = Rlp<'a>;
//
//     fn deref(&self) -> &Rlp<'a> {
//         &self.rlp
//     }
// }

#[self_referencing]
pub struct BytesWithDbValueRef<'a, P> {
    value_ref: DbValueRef<'a>,
    rlp_path: P,
    #[borrows(value_ref)]
    bytes: &'this [u8],
}

// impl<'a> Deref for BytesWithDbValueRef<'a> {
//     type Target = [u8];
//
//     fn deref(&self) -> &[u8] {
//         self.bytes
//     }
// }


pub enum MerkleNode<'a, P> {
    Leaf(Vec<u8>, BytesWithDbValueRef<'a, P>),
    Branch([MerkleValue; 16], Option<BytesWithDbValueRef<'a, P>>),
}

impl<'a, PIn: RlpPath + Copy> MerkleNode<'a, ListNthValue<PIn>> {
    fn decode(rlp: RlpWithDbValueRef<'a, PIn>) -> Self {
        let node = match dbg!(rlp.borrow_rlp().prototype().unwrap()) {
            Prototype::List(2) => {
                let path = ListNthValue::new(rlp.get_path(), 1);
                let new_bytes = BytesWithDbValueRefBuilder {
                    value_ref: rlp.get_ref(),
                    rlp_path: path,
                    bytes_builder: |r: &DbValueRef| path.get_bytes(r)
                }.build(); // TODO: implement rlp.borrow_rlp().at(1).unwrap().data().unwrap()
                MerkleNode::Leaf(vec![], new_bytes)
            }
            Prototype::List(17) => {
                let mut nodes: [MerkleValue; 16] = empty_nodes();
                for (i, node) in nodes.iter_mut().enumerate() {
                    let path = ListNthValue::new(rlp.get_path(), i);
                    let new_rlp = RlpWithDbValueRefBuilder {
                        value_ref: rlp.get_ref(),
                        rlp_path: path,
                        rlp_builder: |r: &DbValueRef| Rlp::new(path.get_bytes(r))
                    }.build(); // TODO: implement rlp.borrow_rlp().at(i).unwrap()
                    *node = MerkleValue::decode(new_rlp);
                }
                let value = if rlp.borrow_rlp().at(16).unwrap().is_empty() {
                    None
                } else {
                    let path = ListNthValue::new(rlp.get_path(), 16);
                    let new_bytes = BytesWithDbValueRefBuilder {
                        value_ref: rlp.get_ref(),
                        rlp_path: path,
                        bytes_builder: |r: &DbValueRef| path.get_bytes(r)
                    }.build(); // TODO: implement rlp.borrow_rlp().at(16).unwrap().data().unwrap()
                    Some(new_bytes)
                };
                MerkleNode::Branch(nodes, value)
            }
            _ => panic!(),
        };
        node
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MerkleValue {
    Empty,
    Hash(H256),
}

impl MerkleValue {
    /// Given a RLP, decode it to a merkle value.
    pub fn decode<P: RlpPath + Copy>(rlp: RlpWithDbValueRef<P>) -> Self {
        if rlp.borrow_rlp().is_empty() {
            return MerkleValue::Empty;
        }

        if rlp.borrow_rlp().size() == 32 {
            return MerkleValue::Hash(rlp.borrow_rlp().as_val().unwrap());
        }

        panic!();
    }
}


fn get_from_db(database: &MyStruct) -> MerkleNode<ListNthValue<rlp_path::Empty>> {
    let bytes = database.get(H256::default());
    let rlp = RlpWithDbValueRefBuilder {
        value_ref: bytes,
        rlp_path: rlp_path::Empty{},
        rlp_builder: |r: &DbValueRef| Rlp::new(r),
    }.build();
    MerkleNode::decode(rlp)
}


fn main() {
    let db = MyStruct::default();

    let mut s = RlpStream::new();
    s.begin_list(2);
    s.append(&"cat").append(&"cat");
    let rlp_inner_bytes = s.out();

    s = RlpStream::new();
    s.begin_list(2);
    s.append(&"cat").append(&rlp_inner_bytes);
    let rlp_bytes = s.out();

    let p = rlp_path::Empty{};
    let p = rlp_path::ListNthValue { parent: p, i: 1 };
    let p = rlp_path::ListNthValue { parent: p, i: 1 };

    dbg!(p.get_bytes(&rlp_bytes));

    // let rlp = Rlp::new(&rlp_bytes);
    // dbg!(rlp.clone());
    // dbg!(rlp.at(1).unwrap());


    db.data.insert(H256::default(), rlp_bytes.to_vec());
    let node = get_from_db(&db);
    match node {
        MerkleNode::Leaf(_, v) => {
            println!("It's a Leaf!");
            dbg!(v.borrow_bytes())
        },
        MerkleNode::Branch(_, Some(v)) => {
            println!("It's a Branch!");
            dbg!(v.borrow_bytes())
        },
        _ => unreachable!(),
    };

    println!("Hello, world!");
}
