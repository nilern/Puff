mod oref;
mod handle;
mod r#type;
mod heap;
mod reader;
mod pos;
mod symbol;
mod string;
mod regs;
mod mutator;
mod heap_obj;
mod bool;
mod list;
mod array;
mod closure;
mod native_fn;
mod builtins;
mod r#box;
mod namespace;
mod bytecode;
mod verifier;
mod anf;
mod cfg;
mod analyzer;
mod compiler;
mod util;

use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline;

use reader::Reader;
use mutator::Mutator;
use compiler::compile;
use closure::Closure;
use verifier::verify;

#[derive(Parser)]
#[command(author, version, about, long_about = None)] // Read from `Cargo.toml`
struct Args {
    #[arg(long)]
    debug: bool
}

const PROMPT: &'static str = "molysite> ";
const HISTORY_FILENAME: &'static str = ".molysite-history.txt";
    
fn main() {
    let debug = Args::parse().debug;

    let mut rl = rustyline::Editor::<()>::new().unwrap();

    if rl.load_history(HISTORY_FILENAME).is_err() {
        println!("No previous history.");
    }

    let mut mt = Mutator::new(1 << 20 /* 1 MiB */).unwrap();

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                let mut reader = Reader::new(line.as_str());

                while let Some(res) = reader.next(&mut mt) {
                    match res {
                        Ok(sv) => {
                            if debug {
                                println!("{}", sv.v.within(&mt));
                                println!("");
                            }

                            let code = compile(&mut mt, *sv.v, debug);

                            if debug {
                                println!("{}", code.within(&mt));
                                println!("");
                            }

                            match unsafe { verify(&mt, code.as_ref()) } {
                                Ok(()) => {
                                    mt.push(code.into());
                                    let f = Closure::new(&mut mt, 0);
                                    mt.pop();
                                    mt.push(f.into());
                                    let v = mt.invoke();

                                    println!("{}", v.within(&mt));
                                },

                                Err(error) => {
                                    println!("VerificationError: {:?}", error);
                                    break;
                                }
                            }
                        },
                        Err(err) => {
                            println!("Error: {:?}", err);
                            break;
                        }
                    }
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    rl.save_history(HISTORY_FILENAME).unwrap();
}
