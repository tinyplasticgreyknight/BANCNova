#![crate_id="disassemble#0.1"]
#![crate_type="bin"]

extern crate bancnova;
use std::os;
use bancnova::syntax::bscode;
use bancnova::syntax::assembly;
use bancnova::syntax::tokenize;
use bancnova::syntax::tokenize::Tokenizer;
use bancnova::syntax::tree::Tree;
use std::io::IoResult;

mod util;

fn main() {
    let args = os::args();
    match disassemble_file(args.get(1).as_slice(), args.get(2).as_slice()) {
        Err(err) => {
            println!("error: [{}] {}", err.kind, err.desc);
            if err.detail.is_some() {
                println!("\t{}", err.detail.unwrap());
            }
            os::set_exit_status(-1);
        },
        _ => {},
    }
}

fn disassemble<R: Reader>(tokenizer: &mut Tokenizer<R>, writer: &mut Writer) -> IoResult<()> {
    let bstree = Tree::parse(tokenizer);
    let bstree: Tree<bscode::Instruction> =
    match bstree {
        Ok(tree) => tree,
        Err(s) => { return util::make_ioerr(s, tokenizer); },
    };
    for bsinst in bstree.iter() {
        let inst = assembly::Instruction::from_bscode(bsinst);
        match writer.write_line(inst.to_str().as_slice()) {
            Err(e) => { return Err(e); },
            _ => {},
        }
    }
    Ok(())
}

fn disassemble_file(infilename: &str, outfilename: &str) -> IoResult<()> {
    let (infile, mut outfile) =
    match util::open_io_files(infilename, outfilename) {
        Ok(p) => p,
        Err(e) => { return Err(e); },
    };

    let mut tokenizer = tokenize::from_file(infile);

    disassemble(&mut tokenizer, &mut outfile)
}
