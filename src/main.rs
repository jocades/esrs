use std::rc::Rc;

use es::Es;

fn test() {
    let mut params = vec!["a", "b", "c"];
    // RC is a reference counted pointer
    let rc = Rc::new(params);
    let rc2 = Rc::clone(&rc);
    println!("{:?}", Rc::strong_count(&rc));

    drop(rc);

    println!("{:?}", Rc::strong_count(&rc2));
}

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
