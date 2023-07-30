use super::cmd::{Command, OneArgOperand, TwoArgOperand, Value, Variable};

fn parse_variable(input: &str) -> Result<Variable, &str> {
    match input {
        "w" => Ok(0),
        "x" => Ok(1),
        "y" => Ok(2),
        "z" => Ok(3),
        _ => Err(input),
    }
}

fn parse_value(input: &str) -> Value {
    if let Ok(v) = parse_variable(input) {
        Value::Variable(v)
    } else {
        Value::Integer(input.parse().unwrap())
    }
}

fn parse_line(input: &str) -> Command {
    let tokens: Vec<&str> = input.split_ascii_whitespace().collect();

    match tokens.len() {
        2 => {
            let var = parse_variable(tokens[1]).unwrap();

            match tokens[0] {
                "inp" => Command::Inp(var),
                other => panic!("Unrecognized one-arg operation {}", other),
            }
        }
        3 => {
            let var = parse_variable(tokens[1]).unwrap();
            let val = parse_value(tokens[2]);

            let op: TwoArgOperand = match tokens[0] {
                "add" => TwoArgOperand::Add,
                "mul" => TwoArgOperand::Mul,
                "div" => TwoArgOperand::Div,
                "mod" => TwoArgOperand::Mod,
                "eql" => TwoArgOperand::Eql,
                other => panic!("Unrecognized two-arg operation {}", other),
            };

            if op == TwoArgOperand::Mod && val == Value::Integer(26) {
                Command::OneArg(var, OneArgOperand::Mod26)
            } else if op == TwoArgOperand::Mul && val == Value::Integer(0) {
                Command::TwoArg(var, TwoArgOperand::Set, Value::Integer(0))
            } else {
                Command::TwoArg(var, op, val)
            }
        }
        other => panic!("Should never get exactly {} tokens in a line (line was {})", other, input),
    }
}

pub fn parse(input: &str) -> Vec<Command> {
    input.lines().map(|line| parse_line(line)).collect()
}
