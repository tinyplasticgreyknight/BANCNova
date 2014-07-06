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
            os::set_exit_status(-1);
        },
        _ => {
            println!("files match");
        },
    }
}

fn read_tree<R: Reader>(tokenizer: &mut Tokenizer<R>) -> IoResult<Tree<bscode::Instruction>> {
    match Tree::parse(tokenizer) {
        Ok(tree) => Ok(tree),
        Err(s) => util::make_ioerr(s, tokenizer),
    }
}

struct ComparisonReporter<'a> {
    filename1: &'a str,
    filename2: &'a str,
    count: uint,
}

impl<'a> ComparisonReporter<'a> {
    fn new(filename1: &'a str, filename2: &'a str) -> ComparisonReporter<'a> {
        ComparisonReporter::<'a> { filename1: filename1, filename2: filename2, count: 0 }
    }

    fn report_general(&mut self, line: uint, message1: &str, message2: &str) {
        println!("{}:{}: {}", self.filename1, line, message1);
        println!("{}:{}: {}", self.filename2, line, message2);
        self.count += 1;
    }

    fn report(&mut self, line: uint, message: &str, inst1: &bscode::Instruction, inst2: &bscode::Instruction) {
        println!("{}:{}: {} {}", self.filename1, line, message, inst1);
        println!("{}:{}: {} {}", self.filename2, line, message, inst2);
        self.count += 1;
    }

    fn end(&self) -> IoResult<()> {
        if self.count == 0 {
            Ok(())
        } else {
            util::make_ioerr_noline("files do not match")
        }
    }
}

fn compare(tree1: Tree<bscode::Instruction>, tree2: Tree<bscode::Instruction>, reporter: &mut ComparisonReporter) -> IoResult<()> {
    let length = min(tree1.len(), tree2.len());
    for i in range(0, length) {
        let inst1 = tree1.get(i);
        let inst2 = tree2.get(i);
        if inst1 != inst2 {
            reporter.report(i+1, "mismatch", inst1, inst2);
        }
    }

    if tree1.len() != tree2.len() {
        reporter.report_general(0, format!("{} line(s)", tree1.len()).as_slice(), format!("{} line(s)", tree2.len()).as_slice() );
    }

    reporter.end()
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

    let mut reporter = ComparisonReporter::new(filename1, filename2);
    match (tree1, tree2) {
        (Ok(tree1), Ok(tree2)) => compare(tree1, tree2, &mut reporter),
        (Err(e), Ok(_)) => Err(e),
        (_, Err(e)) => Err(e),
    }
}
