use std::collections::BinaryHeap;

fn input() -> String {
    std::fs::read_to_string("input/input_23.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

const HALLWAY_WIDTH: usize = 11;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Pos {
    Hall(usize),
    FrontRoom(usize),
    BackRoom(usize),
}

fn a_with_input(input: &str) -> u64 {
    use std::cmp::Reverse;
    use Occupant::*;

    let start_state = parse(input);

    // cost lower bound, cost so far, state
    type PathsQueue = BinaryHeap<Reverse<(u64, State)>>;

    let mut queue = PathsQueue::new();
    queue.push(Reverse((heuristic_finish_cost(start_state), start_state)));

    // TODO: this is too complicated and it's a typo factory
    fn can_move(start: Pos, end: Pos, state: State) -> bool {
        if start == end {
            return false;
        }

        let hall_clear = |x1, x2| {
            if x1 < x2 {
                (x1..=x2).all(|x| state.hall[x] == Empty)
            } else {
                (x2..=x1).all(|x| state.hall[x] == Empty)
            }
        };

        match start {
            Pos::Hall(start_x) => {
                match end {
                    Pos::Hall(_end_x) => {
                        unreachable!("Can't move within a hall");
                        /* todo delete commented out code
                        if start_x < end_x {
                            hall_clear(start_x + 1, end_x)
                        } else if start_x > end_x {
                            hall_clear(end_x, start_x - 1)
                        } else {
                            true
                        }
                         */
                    }
                    Pos::FrontRoom(end_hall) => {
                        let end_x = (end_hall * 2) + 2;
                        state.front_room[end_hall] == Empty && {
                            if start_x < end_x {
                                hall_clear(start_x + 1, end_x)
                            } else if start_x > end_x {
                                hall_clear(end_x, start_x - 1)
                            } else {
                                unreachable!()
                            }
                        }
                    }
                    Pos::BackRoom(end_hall) => {
                        let end_x = (end_hall * 2) + 2;
                        state.front_room[end_hall] == Empty
                            && state.back_room[end_hall] == Empty
                            && {
                                if start_x < end_x {
                                    hall_clear(start_x + 1, end_x)
                                } else if start_x > end_x {
                                    hall_clear(end_x, start_x - 1)
                                } else {
                                    unreachable!()
                                }
                            }
                    }
                }
            }
            Pos::FrontRoom(start_hall) => {
                let start_x = (start_hall * 2) + 2;
                match end {
                    Pos::Hall(end_x) => hall_clear(start_x, end_x),
                    Pos::FrontRoom(end_hall) => {
                        let end_x = (end_hall * 2) + 2;
                        state.front_room[end_hall] == Empty && hall_clear(start_x, end_x)
                    }
                    Pos::BackRoom(end_hall) => {
                        let end_x = (end_hall * 2) + 2;
                        state.front_room[end_hall] == Empty
                            && state.back_room[end_hall] == Empty
                            && hall_clear(start_x, end_x)
                    }
                }
            }
            Pos::BackRoom(start_hall) => {
                let start_x = (start_hall * 2) + 2;
                state.front_room[start_hall] == Empty
                    && match end {
                        Pos::Hall(end_x) => hall_clear(start_x, end_x),
                        Pos::FrontRoom(end_hall) => {
                            let end_x = (end_hall * 2) + 2;
                            state.front_room[end_hall] == Empty && hall_clear(start_x, end_x)
                        }
                        Pos::BackRoom(end_hall) => {
                            let end_x = (end_hall * 2) + 2;
                            state.front_room[end_hall] == Empty
                                && state.back_room[end_hall] == Empty
                                && hall_clear(start_x, end_x)
                        }
                    }
            }
        }
    }

    fn move_cost(start: Pos, end: Pos, occupant: Occupant) -> u64 {
        let tile_cost = match occupant {
            Empty => unreachable!(),
            A => 1,
            B => 10,
            C => 100,
            D => 1000,
        };

        let dist: usize = match end {
            Pos::Hall(end_x) => match start {
                Pos::Hall(start_x) => end_x.abs_diff(start_x),
                Pos::FrontRoom(start_hall) => {
                    let start_x = (start_hall * 2) + 2;
                    end_x.abs_diff(start_x) + 1
                }
                Pos::BackRoom(start_hall) => {
                    let start_x = (start_hall * 2) + 2;
                    end_x.abs_diff(start_x) + 2
                }
            },
            Pos::FrontRoom(end_hall) => {
                let end_x = (end_hall * 2) + 2;
                match start {
                    Pos::Hall(start_x) => end_x.abs_diff(start_x) + 1,
                    Pos::FrontRoom(start_hall) => {
                        if start_hall == end_hall {
                            0
                        } else {
                            let start_x = (start_hall * 2) + 2;
                            end_x.abs_diff(start_x) + 2
                        }
                    }
                    Pos::BackRoom(start_hall) => {
                        if start_hall == end_hall {
                            1
                        } else {
                            let start_x = (start_hall * 2) + 2;
                            end_x.abs_diff(start_x) + 3
                        }
                    }
                }
            }
            Pos::BackRoom(end_hall) => {
                let end_x = (end_hall * 2) + 2;
                match start {
                    Pos::Hall(start_x) => end_x.abs_diff(start_x) + 2,
                    Pos::FrontRoom(start_hall) => {
                        if start_hall == end_hall {
                            1
                        } else {
                            let start_x = (start_hall * 2) + 2;
                            end_x.abs_diff(start_x) + 3
                        }
                    }
                    Pos::BackRoom(start_hall) => {
                        if start_hall == end_hall {
                            0
                        } else {
                            let start_x = (start_hall * 2) + 2;
                            end_x.abs_diff(start_x) + 4
                        }
                    }
                }
            }
        };

        tile_cost * (dist as u64)
    }

    #[inline(always)]
    fn desired_hall(occupant: Occupant) -> Option<usize> {
        match occupant {
            Empty => None,
            A => Some(0),
            B => Some(1),
            C => Some(2),
            D => Some(3),
        }
    }

    'mainloop: while let Some(Reverse((_, state))) = queue.pop() {
        if state.is_complete() {
            return state.cost_so_far;
        }

        // println!("Examining state {:?}", state);

        // for each thing that _can_ move, iterate through their possible moves and enqueue them

        // if an anthropod can move into a final room, it is always optimal to do so, so let's
        // check that first
        for x in 0..HALLWAY_WIDTH {
            let occupant = state.hall[x];
            if let Some(goal_column) = desired_hall(occupant) {
                let start = Pos::Hall(x);

                if can_move(start, Pos::BackRoom(goal_column), state) {
                    let cost = move_cost(start, Pos::BackRoom(goal_column), occupant);
                    let mut new_state = state.clone();
                    new_state.hall[x] = Empty;
                    new_state.back_room[goal_column] = occupant;
                    new_state.cost_so_far += cost;

                    let h = heuristic_finish_cost(new_state);
                    queue.push(Reverse((h, new_state)));

                    assert!(h >= state.cost_so_far);
                    assert_eq!(new_state.occupancy(), 8);

                    /* todo delete block
                    println!("Given state {:?}", state);
                    println!(
                        "  Moved {:?} from hall {} into back room {:?} which is optimal",
                        occupant, x, goal_column
                    );
                    println!("  Resulting state: {:?}", new_state);
                    */

                    continue 'mainloop;
                } else if state.back_room[goal_column] == occupant
                    && can_move(start, Pos::FrontRoom(goal_column), state)
                {
                    let cost = move_cost(start, Pos::FrontRoom(goal_column), occupant);
                    let mut new_state = state.clone();
                    new_state.hall[x] = Empty;
                    new_state.front_room[goal_column] = occupant;
                    new_state.cost_so_far += cost;

                    let h = heuristic_finish_cost(new_state);
                    queue.push(Reverse((h, new_state)));

                    assert!(h >= state.cost_so_far);
                    assert_eq!(new_state.occupancy(), 8);

                    /* todo delete block
                    println!("Given state {:?}", state);
                    println!(
                        "  Moved {:?} from hall {} into front room {:?} which is optimal",
                        occupant, x, goal_column
                    );
                    println!("  Resulting state: {:?}", new_state);
                    */

                    continue 'mainloop;
                }

                // otherwise, it's already in the hallway and it's not allowed to move until
                // it goes into its room, that's rule 3. So we consider more normal movements
            }
        }

        // now the only options are to move an arthopod from a room into the hallway, and
        // we can't short-circuit

        for hall in 0..4 {
            let start = {
                let front = state.front_room[hall];
                if front != Empty {
                    if desired_hall(front).unwrap() != hall
                        || desired_hall(state.back_room[hall]) != Some(hall)
                    {
                        Some((Pos::FrontRoom(hall), front))
                    } else {
                        None
                    }
                } else {
                    let back = state.back_room[hall];
                    if back != Empty && desired_hall(back).unwrap() != hall {
                        Some((Pos::BackRoom(hall), back))
                    } else {
                        None
                    }
                }
            };

            if let Some((start_pos, occupant)) = start {
                // can't sit in front of a door (2, 4, 6, 8) due to Rule
                for x in [0, 1, 3, 5, 7, 9, 10] {
                    let end_pos = Pos::Hall(x);
                    if can_move(start_pos, end_pos, state) {
                        let addl_cost = move_cost(start_pos, end_pos, occupant);
                        let mut new_state = state.clone();
                        new_state.cost_so_far += addl_cost;
                        new_state.hall[x] = occupant;
                        match start_pos {
                            Pos::Hall(_) => unreachable!("prevented by code"),
                            Pos::FrontRoom(_) => new_state.front_room[hall] = Empty,
                            Pos::BackRoom(_) => new_state.back_room[hall] = Empty,
                        }
                        let est_cost = heuristic_finish_cost(new_state);

                        assert!(est_cost >= state.cost_so_far);
                        if new_state.occupancy() != 8 {
                            println!("Old state: {:?}", state);
                            println!("New state: {:?}", new_state);
                            println!("Moved {:?} from {:?} to {:?}", occupant, start_pos, end_pos);
                            assert_eq!(new_state.occupancy(), 8);
                        }
                        assert_eq!(new_state.occupancy(), 8);

                        /* todo delete block
                        println!(
                            "Trying to move {:?} from {:?} to {:?} (running cost {})",
                            occupant, start_pos, end_pos, new_state.cost_so_far
                        );
                        if occupant == C
                            && start_pos == Pos::FrontRoom(1)
                            && end_pos == Pos::Hall(5)
                        {
                            println!("  Original state {:#?}", state);
                            println!("  Resulting state {:#?}", new_state);
                            println!(
                                "  Can (now) move to front_room[2]: {}",
                                can_move(end_pos, Pos::FrontRoom(2), new_state)
                            );
                        }

                         */
                        queue.push(Reverse((est_cost, new_state)));
                    }
                }
            }
        }
    }

    panic!("No solution found")
}

/// Heuristic for the MINIMUM cost required to finish this state.
/// It is not important for this to be completely accurate, however, if this returns a certain
/// number, then the true number should be that estimate OR ABOVE. Algorithms depend on this to
/// function correctly.
fn heuristic_finish_cost(state: State) -> u64 {
    // TODO PERF: probably worth improving this
    state.cost_so_far
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> u64 {
    unimplemented!()
}

#[derive(Copy, Clone, Eq, Debug, PartialEq, Hash, PartialOrd, Ord)]
struct State {
    hall: [Occupant; 11],
    front_room: [Occupant; 4],
    back_room: [Occupant; 4],
    cost_so_far: u64,
}

impl State {
    #[inline]
    fn is_complete(self) -> bool {
        use Occupant::*;

        self.front_room == [A, B, C, D] && self.back_room == [A, B, C, D]
    }

    fn occupancy(self) -> usize {
        self.hall.iter().filter(|&&c| c != Occupant::Empty).count()
            + self
                .front_room
                .iter()
                .filter(|&&c| c != Occupant::Empty)
                .count()
            + self
                .back_room
                .iter()
                .filter(|&&c| c != Occupant::Empty)
                .count()
    }
}

#[derive(Copy, Clone, Eq, Debug, PartialEq, Hash, Ord, PartialOrd)]
enum Occupant {
    Empty,
    A,
    B,
    C,
    D,
}

fn parse(input: &str) -> State {
    fn parse_char(c: Option<char>) -> Occupant {
        match c {
            Some('A') => Occupant::A,
            Some('B') => Occupant::B,
            Some('C') => Occupant::C,
            Some('D') => Occupant::D,
            _ => panic!("bad character {:?}", c),
        }
    }

    let mut lines = input.lines();

    // making sure the sizes here aren't dynamic because the code expects this exactly
    assert_eq!(lines.next(), Some("#############"));
    assert_eq!(lines.next(), Some("#...........#"));

    // line 3
    let line = lines.next().unwrap();
    let mut line_chars = line.chars();
    assert_eq!(line_chars.next(), Some('#'));
    assert_eq!(line_chars.next(), Some('#'));
    assert_eq!(line_chars.next(), Some('#'));
    let o1 = parse_char(line_chars.next());
    assert_eq!(line_chars.next(), Some('#'));
    let o2 = parse_char(line_chars.next());
    assert_eq!(line_chars.next(), Some('#'));
    let o3 = parse_char(line_chars.next());
    assert_eq!(line_chars.next(), Some('#'));
    let o4 = parse_char(line_chars.next());
    assert_eq!(line_chars.next(), Some('#'));
    assert_eq!(line_chars.next(), Some('#'));
    assert_eq!(line_chars.next(), Some('#'));
    assert_eq!(line_chars.next(), None);

    let front_room = [o1, o2, o3, o4];

    // line 4
    let line = lines.next().unwrap();
    let mut line_chars = line.chars();
    assert_eq!(line_chars.next(), Some(' '));
    assert_eq!(line_chars.next(), Some(' '));
    assert_eq!(line_chars.next(), Some('#'));
    let o1 = parse_char(line_chars.next());
    assert_eq!(line_chars.next(), Some('#'));
    let o2 = parse_char(line_chars.next());
    assert_eq!(line_chars.next(), Some('#'));
    let o3 = parse_char(line_chars.next());
    assert_eq!(line_chars.next(), Some('#'));
    let o4 = parse_char(line_chars.next());
    assert_eq!(line_chars.next(), Some('#'));
    assert_eq!(line_chars.next(), None);

    let back_room = [o1, o2, o3, o4];

    assert_eq!(lines.next(), Some("  #########"));
    assert_eq!(lines.next(), None);

    State {
        hall: [Occupant::Empty; 11],
        front_room,
        back_room,
        cost_so_far: 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TRIVIAL_SAMPLE: &'static str = "#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########";

    const EASY_SAMPLE: &'static str = "#############
#...........#
###A#C#B#D###
  #A#B#C#D#
  #########";

    const SAMPLE: &'static str = "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########";

    #[test]
    fn parse_test() {
        use Occupant::*;

        let actual = parse(SAMPLE);

        let expected = State {
            hall: [Empty; 11],
            front_room: [B, C, B, D],
            back_room: [A, D, C, A],
            cost_so_far: 0,
        };

        assert_eq!(actual, expected)
    }

    #[test]
    fn sample_a_1() {
        let actual = a_with_input(TRIVIAL_SAMPLE);
        let expected = 0;

        assert_eq!(actual, expected);
    }

    #[test]
    fn sample_a_2() {
        let actual = a_with_input(EASY_SAMPLE);
        let expected = 460;

        assert_eq!(actual, expected);
    }

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 12521;

        assert_eq!(actual, expected);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(SAMPLE);
        let expected = 2758514936282235;

        assert_eq!(expected, actual);
    }
}
