use std::{
    env, fs,
    io::{Write, stdin, stdout},
    process::exit,
};

use crate::vm::vm::{VM, VMError};

mod vm;

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(args.get(1).unwrap()),
        _ => {
            println!("Usage: lox [script]");
            exit(1);
        }
    }
}

fn run_prompt() {
    let mut vm = VM::default();

    println!("Welcome to \x1b[1mlox-rs\x1b[0m!");

    loop {
        let mut s = String::new();

        print!("\x1b[31;1m>>>\x1b[0m ");
        let _ = stdout().flush();

        stdin().read_line(&mut s).expect("could not read string");

        let mut source = s.trim().to_string();

        // an unfinished statement
        if !source.ends_with(";") && !source.ends_with("}") {
            source += ";";
        }

        match run(&mut vm, &source) {
            // Ok(val) => println!("{}", val),
            Ok(_) => {}
            Err(err) => println!("ERR: {err:?}"),
        }
    }
}

fn run_file(file: &String) {
    let source = fs::read_to_string(file).expect("error reading file");

    let mut vm = VM::default();

    match run(&mut vm, &source) {
        Ok(_) => {}
        Err(err) => panic!("{err:?}"),
    }
}

fn run(vm: &mut VM, code: &String) -> Result<(), VMError> {
    vm.run(code.clone())?;

    Ok(())
}
