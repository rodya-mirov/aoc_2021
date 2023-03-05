use std::collections::HashMap;

fn input() -> String {
    std::fs::read_to_string("input/input_21.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> usize {
    let (mut a_pos, mut b_pos) = parse(input);

    let mut a_score = 0;
    let mut b_score = 0;

    let mut next_roll = 1;
    let mut num_rolls = 0;
    let mut roll = || {
        num_rolls += 1;
        let out = next_roll;
        next_roll += 1;
        while next_roll > 100 {
            next_roll -= 100;
        }
        out
    };

    let mut move_space = |mut pos: usize| {
        let amt = roll() + roll() + roll();
        pos = (pos + amt) % 10;
        if pos == 0 {
            pos = 10;
        }
        pos
    };

    loop {
        // a turn
        a_pos = move_space(a_pos);
        a_score += a_pos;
        if a_score >= 1000 {
            return num_rolls * b_score;
        }

        // b turn
        b_pos = move_space(b_pos);
        b_score += b_pos;
        if b_score >= 1000 {
            return num_rolls * a_score;
        }
    }
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents, 21);

    val.to_string()
}

const MAX_ROLL: usize = 3;

fn b_with_input(input: &str, target_score: u32) -> u128 {
    let (a_pos, b_pos) = parse(input);

    #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
    struct State {
        a_pos: usize,
        b_pos: usize,
        a_score: u32,
        b_score: u32,
    }

    type Cache = HashMap<State, (u128, u128)>;

    let mut cache: Cache = HashMap::new();

    #[inline(always)]
    fn next_pos(pos: usize, roll: usize) -> usize {
        let out = pos + roll;
        if out > 10 {
            out - 10
        } else {
            out
        }
    }

    // finds the number of universes in which each player wins
    fn search_multiverse(state: State, cache: &mut Cache, target_score: u32) -> (u128, u128) {
        if let Some(&(a, b)) = cache.get(&state) {
            return (a, b);
        }

        let mut total_a = 0;
        let mut total_b = 0;

        // for the longest time i forgot that we roll three dice :slam:
        for r1 in 1..MAX_ROLL + 1 {
            for r2 in 1..MAX_ROLL + 1 {
                for r3 in 1..MAX_ROLL + 1 {
                    let roll = r1 + r2 + r3;

                    let new_pos = next_pos(state.a_pos, roll);
                    let new_score = state.a_score + new_pos as u32;
                    if new_score >= target_score {
                        total_a += 1;
                    } else {
                        let flipped_state = State {
                            a_pos: state.b_pos,
                            a_score: state.b_score,
                            b_pos: new_pos,
                            b_score: new_score,
                        };
                        // transposing intentional, this is simulating the other player now
                        // being up to roll
                        let (addl_b, addl_a) =
                            search_multiverse(flipped_state, cache, target_score);
                        total_a += addl_a;
                        total_b += addl_b;
                    }
                }
            }
        }

        cache.insert(state, (total_a, total_b));

        (total_a, total_b)
    }

    let state = State {
        a_pos,
        b_pos,
        a_score: 0,
        b_score: 0,
    };

    let (a_wins, b_wins) = search_multiverse(state, &mut cache, target_score);

    a_wins.max(b_wins)
}

fn parse(input: &str) -> (usize, usize) {
    let mut lines = input.lines();

    let a = lines.next().unwrap();
    let a: String = a
        .chars()
        .skip("Player 1 starting position: ".len())
        .collect();
    let a: usize = a.parse().unwrap();

    let b = lines.next().unwrap();
    let b: String = b
        .chars()
        .skip("Player 2 starting position: ".len())
        .collect();
    let b: usize = b.parse().unwrap();

    assert_eq!(lines.next(), None);

    (a, b)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str = "Player 1 starting position: 4
Player 2 starting position: 8
";

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 739785;

        assert_eq!(actual, expected);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(SAMPLE, 21);
        let expected = 444356092776315;

        assert_eq!(expected, actual);
    }
}
