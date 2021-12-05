use std::fs::File;
use std::io::Read;

pub fn a() -> u64 {
    a_with_input("./src/day4/4a_full.txt")
}

fn a_with_input(path: &str) -> u64 {
    let (calls, mut boards) = parse_input(path);

    for call in calls {
        for board in boards.iter_mut() {
            board.receive_number(call);
            if board.won() {
                return call * board.score();
            }
        }
    }

    panic!("No winner found");
}

pub fn b() -> u64 {
    b_with_input("./src/day4/4a_full.txt")
}

fn b_with_input(path: &str) -> u64 {
    let (calls, mut boards) = parse_input(path);

    let mut last_score = 0;

    for call in calls {
        for board in boards.iter_mut() {
            if board.won() {
                continue;
            }
            board.receive_number(call);
            if board.won() {
                last_score = call * board.score();
            }
        }
    }

    last_score
}

use board::Board;

mod board {
    pub struct Board {
        nums: [[u64; 5]; 5],
        seen: [[bool; 5]; 5],
        won: bool,
    }

    impl Board {
        pub fn from_nums(nums: [[u64; 5]; 5]) -> Board {
            Board {
                nums,
                seen: [[false; 5]; 5],
                won: false,
            }
        }

        pub fn won(&self) -> bool {
            self.won
        }

        pub fn receive_number(&mut self, num: u64) {
            for y in 0..5 {
                for x in 0..5 {
                    if self.nums[y][x] == num && !self.seen[y][x] {
                        self.seen[y][x] = true;
                        self.compute_won(x, y);
                    }
                }
            }
        }

        /// Determine if the board is won. For efficiency, only check rows and columns on
        /// the row / column that actually changed.
        fn compute_won(&mut self, x_changed: usize, y_changed: usize) {
            if self.won {
                return;
            }

            let vertical_match = (0..5).all(|y| self.seen[y][x_changed]);
            if vertical_match {
                self.won = true;
                return;
            }

            let horizontal_match = (0..5).all(|x| self.seen[y_changed][x]);
            if horizontal_match {
                self.won = true;
            }
        }

        pub fn score(&self) -> u64 {
            let mut out = 0;
            for y in 0..5 {
                for x in 0..5 {
                    if !self.seen[y][x] {
                        out += self.nums[y][x];
                    }
                }
            }
            out
        }
    }
}

fn read_file(file_path: &str) -> String {
    let mut f = File::open(file_path).unwrap();
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).unwrap();
    buffer
}

fn parse_input(path: &str) -> (Vec<u64>, Vec<Board>) {
    let text = read_file(path);
    let mut lines = text.lines();

    let calls: Vec<u64> = lines
        .next()
        .unwrap()
        .split(",")
        .map(|t| t.parse::<u64>().unwrap())
        .collect();
    let mut boards = Vec::new();

    while lines.next().is_some() {
        let board = parse_board(&mut lines);
        boards.push(board);
    }

    (calls, boards)
}

fn parse_board<'a, T: Iterator<Item = &'a str>>(lines: &mut T) -> Board {
    let mut next_row = || {
        lines
            .next()
            .unwrap()
            .split_ascii_whitespace()
            .map(|t| t.parse::<u64>().unwrap())
            .collect::<Vec<u64>>()
            .try_into()
            .unwrap()
    };

    let nums = [next_row(), next_row(), next_row(), next_row(), next_row()];

    Board::from_nums(nums)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_sample_test() {
        let expected = 4512;
        let actual = a_with_input("./src/day4/4a_sample.txt");

        assert_eq!(expected, actual);
    }

    #[test]
    fn b_sample_test() {
        let expected = 1924;
        let actual = b_with_input("./src/day4/4a_sample.txt");

        assert_eq!(expected, actual);
    }
}
