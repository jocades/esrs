use es::Es;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 2 {
        println!("Usage: ene [script]");
        std::process::exit(64);
    }

    let mut es = Es::new();

    if args.len() == 2 {
        es.file(&args[1]);
    } else {
        es.repl()
    }
}
