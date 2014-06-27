#![crate_id="disassemble#0.1"]
#![crate_type="bin"]

extern crate bancnova;
use std::os;
use bancnova::syntax::bscode;
use bancnova::syntax::assembly;
use bancnova::syntax::tokenize;
use bancnova::syntax::tokenize::Tokenizer;
use std::io::{File, IoResult, IoError, FileMode, FileAccess, Read, Truncate, Write, Open};

fn main() {
    let args = os::args();
    match disassemble_file(args.get(1).as_slice(), args.get(2).as_slice()) {
        Err(err) => {
            println!("error: [{}] {}", err.kind, err.desc);
            if err.detail.is_some() {
                println!("\t{}", err.detail.unwrap());
            }
        },
        _ => {},
    }
}

fn disassemble<R: Reader>(tokenizer: &mut Tokenizer<R>, writer: &mut Writer) -> IoResult<()> {
    let tree = bscode::Tree::parse(tokenizer).unwrap();
    for bsinst in tree.iter() {
        let inst = assembly::Instruction::from_bscode(bsinst);
        match writer.write_line(inst.to_str().as_slice()) {
            Err(e) => { return Err(e); },
            _ => {},
        }
    }
    Ok(())
}

fn open(filename: &str, mode: FileMode, access: FileAccess) -> IoResult<File> {
    match File::open_mode(&Path::new(filename), mode, access) {
        Ok(f) => {
            Ok(f)
        },
        Err(e) => {
            Err(IoError {
                    kind: e.kind,
                    desc: e.desc,
                    detail: Some(filename.to_string()),
                })
        },
    }
}

fn open_input(filename: &str) -> IoResult<File> {
    open(filename, Open, Read)
}

fn open_output(filename: &str) -> IoResult<File> {
    open(filename, Truncate, Write)
}

fn disassemble_file(infilename: &str, outfilename: &str) -> IoResult<()> {
    let infile  = open_input(infilename);
    let outfile = open_output(outfilename);
    match infile {
        Err(e) => { return Err(e); },
        _ => {},
    }

    match outfile {
        Err(e) => { return Err(e); },
        _ => {},
    }

    let mut tokenizer = tokenize::from_file(infile.unwrap());
    let mut outfile = outfile.unwrap();

    disassemble(&mut tokenizer, &mut outfile)
}
