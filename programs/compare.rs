#![crate_id="compare#0.1"]
#![crate_type="bin"]

extern crate bancnova;
use std::os;
use bancnova::syntax::bscode;
use bancnova::syntax::tokenize;
use bancnova::syntax::tokenize::Tokenizer;
use bancnova::syntax::tree::Tree;
use std::io::IoResult;
use std::cmp::min;

mod util;

fn main() {
    let args = os::args();
    match compare_files(args.get(1).as_slice(), args.get(2).as_slice()) {
        Err(err) => {
            println!("error: [{}] {}", err.kind, err.desc);
            if err.detail.is_some() {
                println!("\t{}", err.detail.unwrap());
            }
        },
        _ => {},
    }
}

fn read_tree<R: Reader>(tokenizer: &mut Tokenizer<R>) -> Tree<bscode::Instruction> {
    Tree::parse(tokenizer).unwrap()
}

fn report_general(message: String) {
    println!("[0] {}", message);
}

fn report(line: uint, message: &str, inst1: &bscode::Instruction, inst2: &bscode::Instruction) {
    println!("[{}] {}", line+1, message);
    println!("{}", inst1.to_str());
    println!("{}", inst2.to_str());
}

fn compare(tree1: Tree<bscode::Instruction>, tree2: Tree<bscode::Instruction>) -> IoResult<()> {
    let length = min(tree1.len(), tree2.len());
    for i in range(0, length) {
        let inst1 = tree1.get(i);
        let inst2 = tree2.get(i);
        if inst1 != inst2 {
            report(i, "lines do not match", inst1, inst2);
        }
    }

    if tree1.len() != tree2.len() {
        report_general(format!("different sizes: {} versus {} instructions", tree1.len(), tree2.len()));
    }

    Ok(())
}

fn compare_files(filename1: &str, filename2: &str) -> IoResult<()> {
    let file1 = match util::open_input(filename1) {
        Ok(p) => p,
        Err(e) => { return Err(e); },
    };
    let file2 = match util::open_input(filename2) {
        Ok(p) => p,
        Err(e) => { return Err(e); },
    };

    let mut tokenizer1 = tokenize::from_file(file1);
    let mut tokenizer2 = tokenize::from_file(file2);

    let tree1 = read_tree(&mut tokenizer1);
    let tree2 = read_tree(&mut tokenizer2);

    compare(tree1, tree2)
}
