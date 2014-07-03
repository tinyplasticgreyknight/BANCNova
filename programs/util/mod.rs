use bancnova::syntax::tokenize::Tokenizer;
use std::io::{IoResult, IoError, OtherIoError};
use std::io::{File, FileMode, FileAccess, Read, Truncate, Write, Open};

#[allow(dead_code)]
pub fn open(filename: &str, mode: FileMode, access: FileAccess) -> IoResult<File> {
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

#[allow(dead_code)]
pub fn open_input(filename: &str) -> IoResult<File> {
    open(filename, Open, Read)
}

#[allow(dead_code)]
pub fn open_output(filename: &str) -> IoResult<File> {
    open(filename, Truncate, Write)
}

#[allow(dead_code)]
pub fn open_io_files(infilename: &str, outfilename: &str) -> IoResult<(File, File)> {
    let infile  = open_input(infilename);
    let outfile = open_output(outfilename);
    match (infile, outfile) {
        (Ok(i), Ok(o)) => Ok((i, o)),
        (Err(e), Ok(_)) => Err(e),
        (_, Err(e)) => Err(e),
    }
}

#[allow(dead_code)]
pub fn make_ioerr<T, R: Reader>(message: &'static str, tokenizer: &Tokenizer<R>) -> IoResult<T> {
    let detail = format!("on line {}", tokenizer.current_line());
    Err(IoError{ kind: OtherIoError, desc: message, detail: Some(detail)})
}
