use crate::helpers::read_file;
use std::fmt;
use std::fmt::{Debug, Formatter};

fn get_input() -> String {
    read_file("input/input_25.txt").expect("Should be able to read file")
}

pub fn a() -> String {
    a_with_input(&get_input()).to_string()
}

fn a_with_input(input: &str) -> usize {
    let mut state = parse(input);

    let width = state.0[0].len();

    for line in &state.0 {
        if line.len() != width {
            panic!("Bad input: inconsistent row width (top line {} but saw {})", width, line.len());
        }
    }

    let mut iters = 1;

    while iterate(&mut state) {
        iters += 1;
    }

    iters
}

fn iterate(state: &mut State) -> bool {
    let width = state.0[0].len();
    let height = state.0.len();

    let mut moves = Vec::new();
    let mut moved = false;

    for (d_pos, goal_occ) in [(Pos { x: 1, y: 0 }, Occupant::East), (Pos { x: 0, y: 1 }, Occupant::South)] {
        let next_pos = |pos: Pos| Pos {
            y: (pos.y + d_pos.y) % height,
            x: (pos.x + d_pos.x) % width,
        };

        moves.clear();

        for y in 0..height {
            for x in 0..width {
                let pos = Pos { x, y };
                let next = next_pos(pos);

                if state.0[pos.y][pos.x] == goal_occ && state.0[next.y][next.x] == Occupant::Empty {
                    moves.push(pos);
                    moved = true;
                }
            }
        }

        for pos in moves.iter().copied() {
            let next = next_pos(pos);
            state.0[pos.y][pos.x] = Occupant::Empty;
            state.0[next.y][next.x] = goal_occ;
        }
    }

    moved
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
struct Pos {
    x: usize,
    y: usize,
}

pub fn b() -> String {
    unimplemented!()
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct State(Vec<Vec<Occupant>>);

impl Debug for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for line in &self.0 {
            for o in line {
                write!(f, "{:?}", o)?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
enum Occupant {
    East,
    South,
    Empty,
}

impl Debug for Occupant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Occupant::East => '>',
                Occupant::South => 'v',
                Occupant::Empty => '.',
            }
        )
    }
}

fn parse(input: &str) -> State {
    fn parse_char(c: char) -> Occupant {
        match c {
            '.' => Occupant::Empty,
            '>' => Occupant::East,
            'v' => Occupant::South,
            _ => panic!("Bad input for occupant: {}", c),
        }
    }

    fn parse_line(line: &str) -> Vec<Occupant> {
        line.chars().map(parse_char).collect()
    }

    State(input.lines().map(parse_line).collect())
}

#[cfg(test)]
mod tests {
    const INPUT: &'static str = "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>";

    const INPUT_1: &'static str = "....>.>v.>
v.v>.>v.v.
>v>>..>v..
>>v>v>.>.v
.>v.v...v.
v>>.>vvv..
..v...>>..
vv...>>vv.
>.v.v..v.v";

    #[test]
    fn sample_a() {
        let expected = 58;
        let actual = super::a_with_input(INPUT);

        assert_eq!(expected, actual)
    }

    #[test]
    fn iter_one() {
        let mut state = super::parse(INPUT);

        let moved = super::iterate(&mut state);

        assert!(moved, "Should have moved");

        let expected = super::parse(INPUT_1);

        assert_eq!(state, expected);
    }

    #[test]
    fn simple_test() {
        let one = "...>>>>>...";
        let two = "...>>>>.>..";

        let mut actual = super::parse(one);
        assert!(super::iterate(&mut actual));
        let expected = super::parse(two);
        assert_eq!(expected, actual);
    }
}
