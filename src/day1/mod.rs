use std::fs::File;
use std::io::Read;

pub fn a() -> u64 {
    a_with_input("./src/day1/1a_full.txt")
}

fn a_with_input(input_file: &str) -> u64 {
    let mut f = File::open(input_file).unwrap();
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).unwrap();

    let mut lines = buffer.lines().map(|line| line.parse::<i64>().unwrap());

    let mut last = lines.next().unwrap();
    let mut increases = 0;

    for next in lines {
        if next > last {
            increases += 1;
        }
        last = next;
    }

    increases
}

pub fn b() -> u64 {
    b_with_input("./src/day1/1a_full.txt")
}

fn b_with_input(input_file: &str) -> u64 {
    let mut f = File::open(input_file).unwrap();
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).unwrap();

    let mut lines = buffer.lines().map(|line| line.parse::<i64>().unwrap());

    let mut a = lines.next().unwrap();
    let mut b = lines.next().unwrap();
    let mut c = lines.next().unwrap();

    let mut last_sum = a + b + c;
    let mut increases = 0;

    for next in lines {
        let next_sum = last_sum - a + next;
        if next_sum > last_sum {
            increases += 1;
        }
        last_sum = next_sum;
        a = b;
        b = c;
        c = next;
    }

    increases
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_1a() {
        let expected = 7;

        let actual = a_with_input("./src/day1/1a_sample.txt");

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_1b() {
        let expected = 5;

        let actual = b_with_input("./src/day1/1a_sample.txt");

        assert_eq!(actual, expected);
    }
}
