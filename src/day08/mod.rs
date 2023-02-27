use std::collections::HashSet;

fn input() -> String {
    std::fs::read_to_string("input/input_08.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> usize {
    let lines: Vec<ParsedLine> = input.lines().map(parse_line).collect();

    // 1 has 2 lines
    // 4 has 4 lines
    // 7 has 3 lines
    // 8 has 7 lines

    // 2, 3, 5 have 5 lines
    // 0 and 6 have 6 lines

    fn is_small(len: usize) -> bool {
        len == 2 || len == 4 || len == 3 || len == 7
    }

    lines
        .iter()
        .map(|ParsedLine(_, ref output)| {
            output
                .iter()
                .map(|s| s.len())
                .filter(|&len| is_small(len))
                .count()
        })
        .sum()
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> u32 {
    input.lines().map(parse_line).map(solve_line).sum()
}

fn solve_line(input: ParsedLine) -> u32 {
    let mut seen: HashSet<Digit> = HashSet::new();

    for tok in input.0 {
        seen.insert(tok);
    }
    for tok in input.1 {
        seen.insert(tok);
    }

    assert_eq!(seen.len(), 10);

    fn only<I, T: Iterator<Item = I>>(mut iter: T) -> I {
        let out = iter.next();
        assert!(out.is_some());
        assert!(iter.next().is_none());
        out.unwrap()
    }

    // some of the digits we can solve trivially
    let is_one = |digit: Digit| digit.len() == 2;
    let is_seven = |digit: Digit| digit.len() == 3;
    let is_four = |digit: Digit| digit.len() == 4;
    let is_eight = |digit: Digit| digit.len() == 7;

    // we can read off which digit is 1 and which is 7 just from their cardinality
    let one_rep: Digit = only(seen.iter().cloned().filter(|&d| is_one(d)));

    // the other two elements of the 1 digit are UR and LR;
    // of those, UR appears in 8 unique digits, and LR appears in 9, so we can distinguish
    let one_bits: [usize; 2] = one_rep.indexes();

    let rep_size =
        |segment_ind: usize| -> usize { seen.iter().filter(|digit| digit.0[segment_ind]).count() };

    let ur_ind: usize = only(one_bits.into_iter().filter(|&ind| rep_size(ind) == 8));
    let lr_ind: usize = only(one_bits.into_iter().filter(|&ind| rep_size(ind) == 9));

    // 5 is the only digit of size 5 without the UR segment, so we can read that off now
    let is_five = |digit: Digit| digit.len() == 5 && !digit.0[ur_ind];

    // 2 is the only digit of size 5 without the LR segment, so we got that too
    let is_two = |digit: Digit| digit.len() == 5 && !digit.0[lr_ind];

    // 3 is the only digit of size 5 with both of those things
    let is_three = |digit: Digit| digit.len() == 5 && digit.0[ur_ind] && digit.0[lr_ind];

    // 6 is the only digit of size 6 without the UR segment
    let is_six = |digit: Digit| digit.len() == 6 && !digit.0[ur_ind];

    // but now to distinguish 9 and 0 we need to get either the LL or Mid segment
    // the Mid segment is present in 7 digits, while the LL segment is present in 4 digits
    // it turns out the LL segment is the only segment present in only 4 digits
    // (while both Mid and Bot are present in 7) so LL is slightly easier to solve for
    let ll_ind: usize = only((0..7).filter(|&ind| rep_size(ind) == 4));

    let is_zero = |digit: Digit| digit.len() == 6 && digit.0[ll_ind] && digit.0[ur_ind];
    let is_nine = |digit: Digit| digit.len() == 6 && !digit.0[ll_ind] && digit.0[ur_ind];

    // Note: the size of data in this problem is really small; it's mostly a non-code puzzle
    // to just figure out what to do. But if we needed to make this really scream for whatever
    // reason we could eliminate a lot of redundant checks above

    let get_val = |digit: Digit| -> u32 {
        if is_zero(digit) {
            0
        } else if is_one(digit) {
            1
        } else if is_two(digit) {
            2
        } else if is_three(digit) {
            3
        } else if is_four(digit) {
            4
        } else if is_five(digit) {
            5
        } else if is_six(digit) {
            6
        } else if is_seven(digit) {
            7
        } else if is_eight(digit) {
            8
        } else if is_nine(digit) {
            9
        } else {
            unreachable!()
        }
    };

    let mut out = 0;

    for d in input.1 {
        out = out * 10 + get_val(d);
    }

    out
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
struct Digit([bool; 7]);

impl Digit {
    fn len(&self) -> usize {
        self.0.iter().filter(|&&b| b).count()
    }

    fn indexes<const N: usize>(&self) -> [usize; N] {
        let mut out = [0; N];

        let mut next_ind = 0;

        for i in (0..self.0.len()).filter(|&ind| self.0[ind]) {
            // panics if next_ind >= N
            out[next_ind] = i;
            next_ind += 1;
        }

        assert_eq!(next_ind, N);

        out
    }
}

struct ParsedLine([Digit; 10], [Digit; 4]);

fn parse_line(line: &str) -> ParsedLine {
    let segments = line.split(" | ").collect::<Vec<_>>();
    assert_eq!(segments.len(), 2);
    let left: Vec<Digit> = segments[0].split_ascii_whitespace().map(to_digit).collect();
    let left: [Digit; 10] = left.try_into().unwrap();
    let right: Vec<Digit> = segments[1].split_ascii_whitespace().map(to_digit).collect();
    let right: [Digit; 4] = right.try_into().unwrap();
    ParsedLine(left, right)
}

fn to_digit(s: &str) -> Digit {
    let mut out = [false; 7];

    for c in s.chars() {
        let ind = (c as usize) - ('a' as usize);
        out[ind] = true;
    }

    Digit(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str =
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce";

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 26;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(SAMPLE);
        let expected = 61229;

        assert_eq!(expected, actual);
    }
}
