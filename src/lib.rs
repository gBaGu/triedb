//! Merkle trie implementation for Ethereum.

use std::{collections::{HashMap, VecDeque}, sync::Arc};
use std::borrow::Borrow;
use std::iter::once;
use std::ops::Deref;

use ouroboros::self_referencing;
use primitive_types::H256;
use rlp::{Rlp, RlpStream};
use sha3::{Digest, Keccak256};

use merkle::{nibble, MerkleNode, MerkleValue};
pub use rocksdb_lib;
pub mod gc;
pub mod merkle;
pub mod state_diff;
pub use memory::*;
pub use mutable::*;

#[cfg(feature = "rocksdb")]
pub mod rocksdb;

mod cache;
mod error;
mod impls;
mod memory;
mod mutable;
mod ops;
mod walker;

use ops::{build, delete, get, insert};

type Result<T> = std::result::Result<T, error::Error>;

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

    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }

    pub fn walk_path<'a>(&self, input: &'a[u8]) -> Rlp<'a> {
        let mut rlp = Rlp::new(input);
        for i in &self.path {
            rlp = rlp.at(*i).unwrap()
        }
        rlp
    }

    pub fn get_subslice<'a>(&self, input: &'a[u8]) -> &'a[u8] {
        if self.is_empty() {
            return input;
        }

        let mut rlp = Rlp::new(input);
        for i in &self.path {
            rlp = rlp.at(*i).unwrap()
        }
        rlp.data().unwrap()
    }
}

#[derive(Clone)]
pub enum DbValueRef<'a> {
    Plain(&'a[u8]),
    Dashmap(Arc<dashmap::mapref::one::Ref<'a, H256, Vec<u8>>>),
    Rocks(Arc<rocksdb_lib::DBPinnableSlice<'a>>),
}

impl<'a> std::fmt::Debug for DbValueRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<'a> PartialEq for DbValueRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.deref().eq(other.deref())
    }
}

impl<'a> Eq for DbValueRef<'a> { }

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

impl<'a> AsRef<[u8]> for DbValueRef<'a> {
    fn as_ref(&self) -> &[u8] {
        match self {
            Self::Plain(bytes) => *bytes,
            Self::Dashmap(r) => r.value(),
            Self::Rocks(slice) => slice.deref(),
        }
    }
}

#[self_referencing]
pub struct RlpWithDbValueRef<'a> {
    value_ref: DbValueRef<'a>,
    rlp_path: RlpPath,
    #[borrows(value_ref, rlp_path)]
    #[covariant]
    rlp: Rlp<'this>,
}

impl<'a> RlpWithDbValueRef<'a> {
    fn get_ref(&self) -> DbValueRef<'a> {
        self.borrow_value_ref().clone()
    }
}

#[self_referencing]
pub struct BytesWithDbValueRef<'a> {
    value_ref: DbValueRef<'a>,
    rlp_path: RlpPath,
    #[borrows(value_ref, rlp_path)]
    bytes: &'this [u8],
}

impl<'a> Clone for BytesWithDbValueRef<'a> {
    fn clone(&self) -> Self {
        make_bytes_wrapper!(self.borrow_value_ref().clone(), self.borrow_rlp_path().clone())
    }
}

impl<'a> Deref for BytesWithDbValueRef<'a> {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        self.borrow_bytes()
    }
}

impl<'a> AsRef<[u8]> for BytesWithDbValueRef<'a> {
    fn as_ref(&self) -> &[u8] {
        self.borrow_bytes()
    }
}

impl<'a> rlp::Encodable for BytesWithDbValueRef<'a> {
    fn rlp_append(&self, s: &mut RlpStream) {
        self.deref().rlp_append(s)
    }
}

impl<'a> std::fmt::Debug for BytesWithDbValueRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<'a> PartialEq for BytesWithDbValueRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.deref().eq(other.deref())
    }
}

impl<'a> Eq for BytesWithDbValueRef<'a> { }

#[macro_export]
macro_rules! make_rlp_wrapper {
    ( $value_ref:expr, $rlp_path:expr ) => {
        {
            RlpWithDbValueRefBuilder {
                value_ref: $value_ref,
                rlp_path: $rlp_path,
                rlp_builder: |r: &DbValueRef, p: &RlpPath| p.walk_path(r)
            }.build()
        }
    };
}

#[macro_export]
macro_rules! make_bytes_wrapper {
    ( $value_ref:expr, $rlp_path:expr ) => {
        {
            BytesWithDbValueRefBuilder {
                value_ref: $value_ref,
                rlp_path: $rlp_path,
                bytes_builder: |r: &DbValueRef, p: &RlpPath| p.get_subslice(r)
            }.build()
        }
    };
}

pub trait Database {
    fn get(&self, key: H256) -> DbValueRef;
}

impl<'a, T: Database> Database for &'a T {
    fn get(&self, key: H256) -> DbValueRef {
        Database::get(*self, key)
    }
}
impl<T: Database> Database for Arc<T> {
    fn get(&self, key: H256) -> DbValueRef {
        Database::get(self.as_ref(), key)
    }
}

/// Change for a merkle trie operation.
#[derive(Default, Debug, Clone)]
pub struct Change {
    /// Additions to the database.
    pub changes: VecDeque<(H256, Option<Vec<u8>>)>,
}

impl Change {
    /// Change to add a new raw value.
    pub fn add_raw(&mut self, key: H256, value: Vec<u8>) {
        self.changes.push_back((key, Some(value)));
    }

    /// Change to add a new node.
    pub fn add_node(&mut self, node: &MerkleNode<'_>) {
        let subnode = rlp::encode(node).to_vec();
        let hash = H256::from_slice(Keccak256::digest(&subnode).as_slice());
        self.add_raw(hash, subnode);
    }

    /// Change to add a new node, and return the value added.
    pub fn add_value<'a, 'b, 'c>(&'a mut self, node: &'c MerkleNode<'b>) -> MerkleValue<'b> {
        if node.inlinable() {
            MerkleValue::Full(Box::new(node.clone()))
        } else {
            let subnode = rlp::encode(node).to_vec();
            let hash = H256::from_slice(Keccak256::digest(&subnode).as_slice());
            self.add_raw(hash, subnode);
            MerkleValue::Hash(hash)
        }
    }

    /// Change to remove a raw key.
    pub fn remove_raw(&mut self, key: H256) {
        self.changes.push_back((key, None));
    }

    /// Change to remove a node. Return whether there's any node being
    /// removed.
    pub fn remove_node(&mut self, node: &MerkleNode<'_>) -> bool {
        if node.inlinable() {
            false
        } else {
            let subnode = rlp::encode(node).to_vec();
            let hash = H256::from_slice(Keccak256::digest(&subnode).as_slice());
            self.remove_raw(hash);
            true
        }
    }

    /// Merge another change to this change.
    pub fn merge(&mut self, other: &Change) {
        for (key, v) in &other.changes {
            if let Some(v) = v {
                self.add_raw(*key, v.clone());
            } else {
                self.remove_raw(*key);
            }
        }
    }

    /// Merge child tree change into this change.
    /// Changes inserts are ordered from child to root, so when we merge child subtree
    /// we should push merge it in front.
    pub fn merge_child(&mut self, other: &Change) {
        for (key, v) in other.changes.iter().rev() {
            self.changes.push_front((*key, v.clone()))
        }
    }
}

/// Get the empty trie hash for merkle trie.
pub fn empty_trie_hash() -> H256 {
    empty_trie_hash!()
}

/// Insert to a merkle trie. Return the new root hash and the changes.
pub fn insert<D: Database>(root: H256, database: &D, key: &[u8], value: &[u8]) -> (H256, Change) {
    let mut change = Change::default();
    let nibble = nibble::from_key(key);

    let (new, subchange) = if root == empty_trie_hash!() {
        let bytes = make_bytes_wrapper!(DbValueRef::Plain(value), RlpPath::default());
        insert::insert_by_empty(nibble, bytes)
    } else {
        let rlp = make_rlp_wrapper!(database.get(root), RlpPath::default());
        let old = MerkleNode::decode(rlp).expect("Unable to decode Node value");
        change.remove_raw(root);
        let bytes = make_bytes_wrapper!(DbValueRef::Plain(value), RlpPath::default());
        insert::insert_by_node(old, nibble, bytes, database)
    };
    change.merge(&subchange);
    change.add_node(&new);

    let hash = H256::from_slice(Keccak256::digest(&rlp::encode(&new).to_vec()).as_slice());
    (hash, change)
}

/// Insert to an empty merkle trie. Return the new root hash and the
/// changes.
pub fn insert_empty<D: Database>(key: &[u8], value: &[u8]) -> (H256, Change) {
    let mut change = Change::default();
    let nibble = nibble::from_key(key);

    let bytes = make_bytes_wrapper!(DbValueRef::Plain(value), RlpPath::default());
    let (new, subchange) = insert::insert_by_empty(nibble, bytes);
    change.merge(&subchange);
    change.add_node(&new);

    let hash = H256::from_slice(Keccak256::digest(&rlp::encode(&new).to_vec()).as_slice());
    (hash, change)
}

/// Delete a key from a markle trie. Return the new root hash and the
/// changes.
pub fn delete<D: Database>(root: H256, database: &D, key: &[u8]) -> (H256, Change) {
    let mut change = Change::default();
    let nibble = nibble::from_key(key);

    let (new, subchange) = if root == empty_trie_hash!() {
        return (root, change);
    } else {
        let rlp = make_rlp_wrapper!(database.get(root), RlpPath::default());
        let old = MerkleNode::decode(rlp).expect("Unable to decode Node value");
        change.remove_raw(root);
        delete::delete_by_node(old, nibble, database)
    };
    change.merge(&subchange);

    match new {
        Some(new) => {
            change.add_node(&new);

            let hash = H256::from_slice(Keccak256::digest(&rlp::encode(&new).to_vec()).as_slice());
            (hash, change)
        }
        None => (empty_trie_hash!(), change),
    }
}

/// Build a merkle trie from a map. Return the root hash and the
/// changes.
pub fn build(map: &HashMap<Vec<u8>, Vec<u8>>) -> (H256, Change) {
    let mut change = Change::default();

    if map.is_empty() {
        return (empty_trie_hash!(), change);
    }

    let mut node_map = HashMap::new();
    for (key, value) in map {
        let bytes = make_bytes_wrapper!(DbValueRef::Plain(value), RlpPath::default());
        node_map.insert(nibble::from_key(key.as_ref()), bytes);
    }

    let (node, subchange) = build::build_node(&node_map);
    change.merge(&subchange);
    change.add_node(&node);

    let hash = H256::from_slice(Keccak256::digest(&rlp::encode(&node).to_vec()).as_slice());
    (hash, change)
}

/// Get a value given the root hash and the database.
pub fn get<'a, 'b, D: Database>(root: H256, database: &'a D, key: &'b [u8]) -> Option<BytesWithDbValueRef<'a>> {
    if root == empty_trie_hash!() {
        None
    } else {
        let nibble = nibble::from_key(key);
        let rlp = make_rlp_wrapper!(database.get(root), RlpPath::default());
        let node = MerkleNode::decode(rlp).expect("Unable to decode Node value");
        get::get_by_node(node, nibble, database)
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! empty_trie_hash {
    () => {{
        use std::str::FromStr;

        H256::from_str("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421").unwrap()
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    const KECCAK_NULL_RLP: H256 = H256([
        0x56, 0xe8, 0x1f, 0x17, 0x1b, 0xcc, 0x55, 0xa6, 0xff, 0x83, 0x45, 0xe6, 0x92, 0xc0, 0xf8,
        0x6e, 0x5b, 0x48, 0xe0, 0x1b, 0x99, 0x6c, 0xad, 0xc0, 0x01, 0x62, 0x2f, 0xb5, 0xe3, 0x63,
        0xb4, 0x21,
    ]);

    #[test]
    fn it_checks_macro_generates_expected_empty_hash() {
        assert_eq!(empty_trie_hash!(), KECCAK_NULL_RLP);
    }
}
