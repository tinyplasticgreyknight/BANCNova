use std::io::{File, BufReader};
use std::container::Container;
use std::io::{BufferedWriter};
use std::fmt::{Show, Formatter, FormatError};
use syntax::tokenize::{Tokenizer};
use result::BancResult;

pub trait TreeNode : Show {
    fn parse<R: Reader>(tokenizer: &mut Tokenizer<R>) -> BancResult<Option<Self>>;
}

#[deriving(PartialEq,Eq)]
pub struct Tree<I> {
    instructions: Vec<I>,
}

pub struct TreeIterator<'a, I> {
    tree: &'a Tree<I>,
    i: uint,
}

impl<I: TreeNode> Tree<I> {
    pub fn new() -> Tree<I> {
        Tree{ instructions: vec!() }
    }

    pub fn push(&mut self, inst: I) {
        self.instructions.push(inst);
    }

    pub fn get<'a>(&'a self, index: uint) -> &'a I {
        self.instructions.get(index)
    }

    pub fn parse_string(text: &str) -> BancResult<Tree<I>> {
        let sreader = BufReader::new(text.as_bytes());
        let mut tokenizer = Tokenizer::<BufReader>::from_buf(sreader);
        Tree::parse(&mut tokenizer)
    }

    pub fn parse_file(file: File) -> BancResult<Tree<I>> {
        let mut tokenizer = Tokenizer::<File>::from_file(file);
        Tree::parse(&mut tokenizer)
    }

    pub fn parse<R: Reader>(tokenizer: &mut Tokenizer<R>) -> BancResult<Tree<I>> {
        let mut tree = Tree::new();
        while ! tokenizer.eof() {
            let node: BancResult<Option<I>> = TreeNode::parse(tokenizer);
            match node {
                Ok(Some(inst)) => {
                    tree.push(inst);
                },
                Ok(None) => {
                    break;
                },
                Err(e) => {
                    return Err(e);
                },
            }
        }
        Ok(tree)
    }

    pub fn render<W: Writer>(&self, buffer: &mut BufferedWriter<W>) {
        buffer.write_str(self.to_str().as_slice()).unwrap();
    }

    pub fn iter<'a>(&'a self) -> TreeIterator<'a, I> {
        TreeIterator::new(self, 0)
    }
}

impl<I: TreeNode> Container for Tree<I> {
    fn len(&self) -> uint {
        self.instructions.len()
    }
    fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }
}

impl<'a, I: TreeNode> TreeIterator<'a, I> {
    fn new(tree: &'a Tree<I>, start: uint) -> TreeIterator<'a, I> {
        TreeIterator {
            tree: tree,
            i: start,
        }
    }

    fn maybe_get(&self, i: uint) -> Option<&'a I> {
        if self.i < self.tree.len() {
            Some(self.tree.get(i))
        } else {
            None
        }
    }
}

impl<'a, I: TreeNode> Iterator<&'a I> for TreeIterator<'a, I> {
    fn next(&mut self) -> Option<&'a I> {
        let o = self.maybe_get(self.i);
        self.i += 1;
        o
    }
}

impl<'a, I: TreeNode> RandomAccessIterator<&'a I> for TreeIterator<'a, I> {
    fn indexable(&self) -> uint {
        self.tree.len()
    }
    fn idx(&mut self, i: uint) -> Option<&'a I> {
        self.maybe_get(i)
    }
}

impl<I: TreeNode> Show for Tree<I> {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        for inst in self.instructions.iter() {
            inst.fmt(formatter);
            formatter.write_char('\n');
        }
        Ok(())
    }
}
