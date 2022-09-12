mod oref;
mod handle;
mod r#type;
mod heap;
mod reader;
mod pos;
mod symbol;
mod mutator;

use rustyline::error::ReadlineError;
use rustyline;

use reader::Reader;

const PROMPT: &'static str = "molysite> ";
const HISTORY_FILENAME: &'static str = ".molysite-history.txt";
    
fn main() -> rustyline::Result<()> {
    let mut rl = rustyline::Editor::<()>::new()?;

    if rl.load_history(HISTORY_FILENAME).is_err() {
        println!("No previous history.");
    }

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                let reader = Reader::new(line.as_str());

                for res in reader {
                    match res {
                        Ok(sv) => println!("{}", sv.v),
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

    rl.save_history(HISTORY_FILENAME)
}
