use std::fs::File;
use std::io::Read;
use std::str::FromStr;

pub fn a() -> u64 {
    a_with_input("./src/day2/2a_full.txt")
}

fn a_with_input(input_file: &str) -> u64 {
    let commands = read_file(input_file);
    let mut x = 0;
    let mut y = 0;
    for command in commands {
        match command.0 {
            Command::Up => y -= command.1,
            Command::Down => y += command.1,
            Command::Forward => x += command.1,
        }
    }
    (x.abs() as u64) * (y.abs() as u64)
}

pub fn b() -> u64 {
    b_with_input("./src/day2/2a_full.txt")
}

fn b_with_input(input_file: &str) -> u64 {
    let commands = read_file(input_file);
    let mut x = 0;
    let mut y = 0;
    let mut aim = 0;
    for command in commands {
        match command.0 {
            Command::Up => aim -= command.1,
            Command::Down => aim += command.1,
            Command::Forward => {
                x += command.1;
                y += command.1 * aim;
            }
        }
    }
    (x.abs() as u64) * (y.abs() as u64)
}

fn read_file(file_path: &str) -> Vec<FullCommand> {
    let mut f = File::open(file_path).unwrap();
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).unwrap();

    buffer
        .lines()
        .map(|line| line.parse::<FullCommand>().unwrap())
        .collect()
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum Command {
    Up,
    Down,
    Forward,
}

impl FromStr for Command {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "down" => Ok(Command::Down),
            "forward" => Ok(Command::Forward),
            "up" => Ok(Command::Up),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct FullCommand(Command, i64);

impl FromStr for FullCommand {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_ascii_whitespace();
        let command: Command = tokens.next().ok_or(())?.parse()?;
        let val: i64 = tokens.next().ok_or(())?.parse().map_err(|_| ())?;
        if tokens.next().is_some() {
            Err(())
        } else {
            Ok(FullCommand(command, val))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_2a() {
        let expected = 150;

        let actual = a_with_input("./src/day2/2a_sample.txt");

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_2b() {
        let expected = 900;

        let actual = b_with_input("./src/day2/2a_sample.txt");

        assert_eq!(actual, expected);
    }
}
