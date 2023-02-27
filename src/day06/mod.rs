use crate::helpers::read_file;

pub fn a() -> usize {
    op_with_input("./src/day06/full.txt", 80)
}

pub fn b() -> usize {
    op_with_input("./src/day06/full.txt", 256)
}

fn op_with_input(path: &str, days: usize) -> usize {
    let mut state = parse(path);

    for _ in 0..days {
        state.next();
    }

    state.num_fish()
}

fn parse(path: &str) -> State {
    let read = read_file(path).unwrap();
    let mut state = State::default();

    for tok in read.split(',') {
        let rem: usize = tok.parse().unwrap();
        state.count[rem] += 1;
    }

    state
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Default)]
struct State {
    count: [usize; 10],
}

impl State {
    fn num_fish(&self) -> usize {
        self.count.iter().copied().sum()
    }

    fn next(&mut self) {
        let mut next = State::default();

        next.count[8] += self.count[0];
        next.count[6] += self.count[0];

        for i in 1..self.count.len() {
            next.count[i - 1] += self.count[i];
        }

        *self = next;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_sample_1() {
        let actual = op_with_input("./src/day06/sample.txt", 18);
        let expected = 26;

        assert_eq!(actual, expected);
    }

    #[test]
    fn a_sample_2() {
        let actual = op_with_input("./src/day06/sample.txt", 80);
        let expected = 5934;

        assert_eq!(actual, expected);
    }

    #[test]
    fn b_sample() {
        let actual = op_with_input("./src/day06/sample.txt", 256);
        let expected = 26984457539;

        assert_eq!(actual, expected);
    }
}
