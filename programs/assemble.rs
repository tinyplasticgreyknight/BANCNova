#![crate_id="assemble#0.1"]
#![crate_type="bin"]

extern crate bancnova;
use std::os;
use bancnova::syntax::assembly;
use bancnova::syntax::tokenize;
use bancnova::syntax::tokenize::Tokenizer;
use bancnova::syntax::tree::Tree;
use std::io::IoResult;

mod util;

fn main() {
    let args = os::args();
    let infilename = args.get(1).as_slice();
    let outfilename = args.get(2).as_slice();
    match assemble_file(infilename, outfilename) {
        Err(err) => {
            println!("[{}] {}", err.kind, err.desc);
            if err.detail.is_some() {
                println!("\t{}", err.detail.unwrap());
            }
            os::set_exit_status(-1);
        },
        _ => {},
    }
}

fn assemble<R: Reader>(tokenizer: &mut Tokenizer<R>, writer: &mut Writer) -> IoResult<()> {
    let asmtree = Tree::parse(tokenizer);
    let asmtree: Tree<assembly::Instruction> =
    match asmtree {
        Ok(tree) => tree,
        Err(s) => { return util::make_ioerr(s, tokenizer); },
    };
    for asminst in asmtree.iter() {
        let bsinst = asminst.as_bscode();
        match writer.write_line(bsinst.to_str().as_slice()) {
            Err(e) => { return Err(e); },
            _ => {},
        }
    }
    Ok(())
}

fn assemble_file(infilename: &str, outfilename: &str) -> IoResult<()> {
    let (infile, mut outfile) =
    match util::open_io_files(infilename, outfilename) {
        Ok(p) => p,
        Err(e) => { return Err(e); },
    };

    let mut tokenizer = tokenize::from_file(infile);

    assemble(&mut tokenizer, &mut outfile)
}
