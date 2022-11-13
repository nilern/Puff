mod oref;
mod handle;
mod fixnum;
mod integers;
mod flonum;
mod char;
mod r#type;
mod heap;
mod syntax;
mod reader;
mod symbol;
mod string;
mod regs;
mod mutator;
mod heap_obj;
mod bool;
mod list;
mod vector;
mod closure;
mod native_fn;
mod continuation;
mod write;
mod builtins;
mod r#box;
mod namespace;
mod bytecode;
mod verifier;
mod compiler;
mod util;

use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline;

use oref::ORef;
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

    let mut mt = Mutator::new(1 << 20 /* 1 MiB */, debug).unwrap();

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                let mut reader = Reader::new(line.as_str(), None);

                while let Some(res) = reader.next(&mut mt) {
                    match res {
                        Ok(stx) => {
                            if debug {
                                println!("{}", ORef::from(stx.oref()).within(&mt));
                                println!("");
                            }

                            let code = compile(&mut mt, stx.into(), debug);

                            if debug {
                                println!("{}", code.within(&mt));
                                println!("");
                            }

                            match verify(&mt, mt.borrow(code)) {
                                Ok(()) => {
                                    mt.push(code.into());
                                    let f = Closure::new(&mut mt, 0);
                                    mt.pop();
                                    mt.push(f.into());
                                    let vs: Vec<ORef> = mt.invoke().iter().copied().collect();
                                    mt.regs_mut().truncate(0);

                                    for v in vs {
                                        println!("{}", v.within(&mt));
                                    }
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
