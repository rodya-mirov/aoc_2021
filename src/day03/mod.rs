use std::fs::File;
use std::io::Read;

pub fn a() -> u64 {
    a_with_input("./src/day03/3a_full.txt", 12)
}

fn a_with_input(input_file: &str, input_length: usize) -> u64 {
    let vals = read_file(input_file);

    // idx -> num lines with bit one at that idx
    let mut one_counts: Vec<usize> = vec![0; input_length];

    let num_rows = vals.len();

    for mut val in vals {
        let mut idx = 0;
        while val > 0 {
            if (val & 1) > 0 {
                one_counts[idx] += 1;
            }
            val >>= 1;
            idx += 1;
        }
    }

    let mut epsilon: u64 = 0;
    let mut gamma: u64 = 0;

    for bit in 0..one_counts.len() {
        let magnitude = 1 << bit;

        // it doesn't tell us how to break ties, so i'll assume it doesn't matter
        if one_counts[bit] * 2 > num_rows {
            gamma += magnitude;
        } else {
            epsilon += magnitude;
        }
    }

    epsilon * gamma
}

pub fn b() -> u64 {
    b_with_input("./src/day3/3a_full.txt", 12)
}

fn b_with_input(input_file: &str, input_length: usize) -> u64 {
    let numbers = read_file(input_file);

    let oxygen = compute_oxygen(&numbers, input_length, 0);
    let scrubber = compute_scrubber(&numbers, input_length, 0);

    oxygen * scrubber
}

fn compute_oxygen(numbers: &[u64], input_length: usize, bit_idx: usize) -> u64 {
    if numbers.len() == 1 {
        return numbers[0];
    }

    if numbers.is_empty() || bit_idx >= input_length {
        panic!();
    }

    let bit_flag = 1 << (input_length - bit_idx - 1);

    let mut ones = 0;
    let original_length = numbers.len();

    for n in numbers {
        if (n & bit_flag) > 0 {
            ones += 1;
        }
    }

    let filtered: Vec<u64> = if ones * 2 >= original_length {
        numbers
            .iter()
            .copied()
            .filter(|n| (n & bit_flag) > 0)
            .collect()
    } else {
        numbers
            .iter()
            .copied()
            .filter(|n| (n & bit_flag) == 0)
            .collect()
    };

    compute_oxygen(&filtered, input_length, bit_idx + 1)
}

fn compute_scrubber(numbers: &[u64], input_length: usize, bit_idx: usize) -> u64 {
    if numbers.len() == 1 {
        return numbers[0];
    }

    if numbers.is_empty() || bit_idx >= input_length {
        panic!();
    }

    let bit_flag = 1 << (input_length - bit_idx - 1);

    let mut ones = 0;
    let original_length = numbers.len();

    for n in numbers {
        if (n & bit_flag) > 0 {
            ones += 1;
        }
    }

    let filtered: Vec<u64> = if ones * 2 < original_length {
        numbers
            .iter()
            .copied()
            .filter(|n| (n & bit_flag) > 0)
            .collect()
    } else {
        numbers
            .iter()
            .copied()
            .filter(|n| (n & bit_flag) == 0)
            .collect()
    };

    compute_scrubber(&filtered, input_length, bit_idx + 1)
}

fn read_file(file_path: &str) -> Vec<u64> {
    let mut f = File::open(file_path).unwrap();
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).unwrap();

    buffer
        .lines()
        .map(|line| u64::from_str_radix(line, 2).unwrap())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_3a() {
        let expected = 198;

        let actual = a_with_input("./src/day03/3a_sample.txt", 5);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_3b() {
        let expected = 230;

        let actual = b_with_input("./src/day03/3a_sample.txt", 5);

        assert_eq!(actual, expected);
    }
}
