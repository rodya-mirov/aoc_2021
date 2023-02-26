fn input() -> String {
    std::fs::read_to_string("input/input_10.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> u64 {
    input.lines().map(corruption_score).sum()
}

// returns 0 if no corruption
fn corruption_score(line: &str) -> u64 {
    let mut block_stack = Vec::new();

    enum State {
        Corrupt(u64),
        Incomplete,
        GoodSoFar,
    }

    fn expect_char(stack: &mut Vec<char>, exp: char, points: u64) -> State {
        if stack.is_empty() {
            return State::Incomplete;
        }

        let act = stack.pop().unwrap();
        if act == exp {
            State::GoodSoFar
        } else {
            State::Corrupt(points)
        }
    }

    for c in line.chars() {
        let score: State = match c {
            '(' | '[' | '{' | '<' => {
                block_stack.push(c);
                State::GoodSoFar
            }
            ')' => expect_char(&mut block_stack, '(', 3),
            ']' => expect_char(&mut block_stack, '[', 57),
            '}' => expect_char(&mut block_stack, '{', 1197),
            '>' => expect_char(&mut block_stack, '<', 25137),
            _ => unreachable!("Unexpected character: {}", c),
        };

        match score {
            State::Corrupt(c) => return c,
            State::Incomplete => return 0,
            _ => {}
        }
    }

    if block_stack.is_empty() {
        unreachable!(
            "Every line should be bad, but found no issue with: {}",
            line
        )
    }

    // but now this is incomplete
    0
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> u64 {
    let mut scores: Vec<u64> = input.lines().filter_map(completion_score).collect();

    scores.sort();

    let len = scores.len();

    assert_eq!(len % 2, 1);

    scores[(len - 1) / 2]
}

// returns None if corrupt
fn completion_score(line: &str) -> Option<u64> {
    let mut block_stack = Vec::new();

    fn score(mut stack: Vec<char>) -> u64 {
        let mut out = 0;
        while let Some(c) = stack.pop() {
            out *= 5;
            out += match c {
                '(' => 1,
                '[' => 2,
                '{' => 3,
                '<' => 4,
                _ => unreachable!("Bad char: {}", c),
            };
        }
        out
    }

    enum State {
        Corrupt,
        Incomplete,
        GoodSoFar,
    }

    fn expect_char(stack: &mut Vec<char>, exp: char) -> State {
        if stack.is_empty() {
            return State::Incomplete;
        }

        let act = stack.pop().unwrap();
        if act == exp {
            State::GoodSoFar
        } else {
            State::Corrupt
        }
    }

    for c in line.chars() {
        let state: State = match c {
            '(' | '[' | '{' | '<' => {
                block_stack.push(c);
                State::GoodSoFar
            }
            ')' => expect_char(&mut block_stack, '('),
            ']' => expect_char(&mut block_stack, '['),
            '}' => expect_char(&mut block_stack, '{'),
            '>' => expect_char(&mut block_stack, '<'),
            _ => unreachable!("Unexpected character: {}", c),
        };

        match state {
            State::Corrupt => return None,
            State::Incomplete => return Some(score(block_stack)),
            _ => {}
        }
    }

    if block_stack.is_empty() {
        unreachable!(
            "Every line should be bad, but found no issue with: {}",
            line
        )
    }

    // but now this is incomplete
    Some(score(block_stack))
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]";

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 26397;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a_1() {
        let actual = corruption_score("{([(<{}[<>[]}>{[]{[(<()>");
        let expected = 1197;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a_2() {
        let actual = corruption_score("[[<[([]))<([[{}[[()]]]");
        let expected = 3;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a_3() {
        let actual = corruption_score("[{[{({}]{}}([{[{{{}}([]");
        let expected = 57;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a_4() {
        let actual = corruption_score("[<(<(<(<{}))><([]([]()");
        let expected = 3;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a_5() {
        let actual = corruption_score("<{([([[(<>()){}]>(<<{{");
        let expected = 25137;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(SAMPLE);
        let expected = 288957;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b_1() {
        let actual = completion_score("[({(<(())[]>[[{[]{<()<>>");
        let expected = 288957;

        assert_eq!(expected, actual.unwrap());
    }

    #[test]
    fn sample_b_2() {
        let actual = completion_score("[(()[<>])]({[<{<<[]>>(");
        let expected = 5566;

        assert_eq!(expected, actual.unwrap());
    }

    #[test]
    fn sample_b_3() {
        let actual = completion_score("(((({<>}<{<{<>}{[]{[]{}");
        let expected = 1480781;

        assert_eq!(expected, actual.unwrap());
    }

    #[test]
    fn sample_b_4() {
        let actual = completion_score("{<[[]]>}<{[{[{[]{()[[[]");
        let expected = 995444;

        assert_eq!(expected, actual.unwrap());
    }

    #[test]
    fn sample_b_5() {
        let actual = completion_score("<{([{{}}[<[[[<>{}]]]>[]]");
        let expected = 294;

        assert_eq!(expected, actual.unwrap());
    }
}
