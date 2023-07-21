use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::HashSet;

fn input() -> String {
    std::fs::read_to_string("input/input_23.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

const HALLWAY_WIDTH: usize = 11;
const ALLOWED_HALLWAY_POSITIONS: [usize; 7] = [0, 1, 3, 5, 7, 9, 10];

fn a_with_input(input: &str) -> u64 {
    let state: State<2> = parse(input);
    best_cost(state)
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> u64 {
    let state: State<2> = parse(input);
    let state: State<4> = state.unfold();
    best_cost(state)
}

fn best_cost<const N: usize>(start_state: State<N>) -> u64 {
    let mut to_process: BinaryHeap<Reverse<(u64, State<N>)>> = BinaryHeap::new();
    let mut seen: HashSet<State<N>> = HashSet::new();

    to_process.push(Reverse((0, start_state)));

    let mut buffer: Vec<(u64, State<N>)> = Vec::new();

    while let Some(Reverse((cost, next_state))) = to_process.pop() {
        // we iterate in lowest-cost-first order, so if we've seen a configuration before, this
        // is not optimal
        if !seen.insert(next_state) {
            continue;
        }

        if next_state.is_complete() {
            return cost;
        }

        buffer.clear();
        set_neighbors(next_state, &mut buffer);

        for (addl_cost, new_state) in buffer.iter().copied() {
            to_process.push(Reverse((addl_cost + cost, new_state)));
        }
    }

    panic!("No solution found")
}

enum Position {
    // position in the hallways
    Hallway(usize),
    // position in a room; this is (room_idx, room_pos); so room_idx=0 is left, and room_pos=0 is front
    Room(usize, usize),
}

/// For each state accessible from this state, adds an element to the buffer which is
/// of the form (additional_cost, state_after_move)
fn set_neighbors<const N: usize>(state: State<N>, buffer: &mut Vec<(u64, State<N>)>) {
    let rooms_available = [
        state.room_ready(0),
        state.room_ready(1),
        state.room_ready(2),
        state.room_ready(3),
    ];

    // first, check for dudes in the hallway
    for hallway_pos in 0..HALLWAY_WIDTH {
        let occ = state.hall[hallway_pos];
        if occ != Occupant::Empty {
            let desired_room = occ.desired_room();
            if rooms_available[desired_room] {
                let room = state.rooms[desired_room];
                let goal_room_pos = (0..N)
                    .filter(|&room_pos| room.occupants[room_pos] == Occupant::Empty)
                    .last()
                    .expect("If the room is ready there should be a spot in it");
                let start_pos = Position::Hallway(hallway_pos);
                let goal_pos = Position::Room(desired_room, goal_room_pos);

                if let Some(dist) = state.path_length(start_pos, goal_pos) {
                    let tile_cost = occ.per_tile_cost();
                    let addl_cost = dist * tile_cost;
                    let mut new_state = state.clone();
                    std::mem::swap(
                        &mut new_state.hall[hallway_pos],
                        &mut new_state.rooms[desired_room].occupants[goal_room_pos],
                    );
                    buffer.push((addl_cost, new_state));
                }
            }
        }
    }

    // then check moves into the hallway
    for room_idx in 0..4 {
        // never need to move anybody out of a room that's already "done"
        if rooms_available[room_idx] {
            continue;
        }

        let room = state.rooms[room_idx];

        if let Some(room_pos) = (0..N)
            .filter(|&room_pos| room.occupants[room_pos] != Occupant::Empty)
            .next()
        {
            let occ = room.occupants[room_pos];

            for hallway_pos in ALLOWED_HALLWAY_POSITIONS.iter().copied() {
                let start_pos = Position::Room(room_idx, room_pos);
                let goal_pos = Position::Hallway(hallway_pos);

                if let Some(dist) = state.path_length(start_pos, goal_pos) {
                    let tile_cost = occ.per_tile_cost();
                    let addl_cost = dist * tile_cost;
                    let mut new_state = state.clone();
                    std::mem::swap(
                        &mut new_state.hall[hallway_pos],
                        &mut new_state.rooms[room_idx].occupants[room_pos],
                    );
                    buffer.push((addl_cost, new_state));
                }
            }
        }
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

impl Occupant {
    #[inline(always)]
    fn per_tile_cost(self) -> u64 {
        match self {
            Occupant::A => 1,
            Occupant::B => 10,
            Occupant::C => 100,
            Occupant::D => 1000,
            Occupant::Empty => panic!("Illegal argument, don't be zero"),
        }
    }

    #[inline(always)]
    fn desired_room(self) -> usize {
        match self {
            Occupant::A => 0,
            Occupant::B => 1,
            Occupant::C => 2,
            Occupant::D => 3,
            Occupant::Empty => panic!("Illegal argument, don't be zero"),
        }
    }
}

impl Default for Occupant {
    fn default() -> Self {
        Occupant::Empty
    }
}

#[derive(Copy, Clone, Eq, Debug, PartialEq, Hash, PartialOrd, Ord, Default)]
struct State<const N: usize> {
    hall: [Occupant; HALLWAY_WIDTH],
    rooms: [Room<N>; 4],
}

#[derive(Copy, Clone, Eq, Debug, PartialEq, Hash, PartialOrd, Ord)]
struct Room<const N: usize> {
    // 0 is the front, 1 is next, etc. to the back
    occupants: [Occupant; N],
}

impl<const N: usize> Room<N> {
    #[inline(always)]
    fn matches_goal(self, target: Occupant) -> bool {
        self.occupants
            .iter()
            .copied()
            .all(|c| c == Occupant::Empty || c == target)
    }
}

impl State<2> {
    fn unfold(self: State<2>) -> State<4> {
        let mut out: State<4> = State::default();

        for room_idx in 0..4 {
            out.rooms[room_idx].occupants[0] = self.rooms[room_idx].occupants[0];
            out.rooms[room_idx].occupants[3] = self.rooms[room_idx].occupants[1];
        }

        out.rooms[0].occupants[1] = Occupant::D;
        out.rooms[0].occupants[2] = Occupant::D;

        out.rooms[1].occupants[1] = Occupant::C;
        out.rooms[1].occupants[2] = Occupant::B;

        out.rooms[2].occupants[1] = Occupant::B;
        out.rooms[2].occupants[2] = Occupant::A;

        out.rooms[3].occupants[1] = Occupant::A;
        out.rooms[3].occupants[2] = Occupant::C;

        out
    }
}

impl<const N: usize> Default for Room<N> {
    fn default() -> Self {
        Self {
            occupants: [Occupant::default(); N],
        }
    }
}

impl<const N: usize> State<N> {
    #[inline]
    fn is_complete(self) -> bool {
        use Occupant::*;

        self.rooms[0].occupants == [A; N]
            && self.rooms[1].occupants == [B; N]
            && self.rooms[2].occupants == [C; N]
            && self.rooms[3].occupants == [D; N]
    }

    /// Whether the room at [room_idx] is ready for people to move in, instead of out
    fn room_ready(self, room_idx: usize) -> bool {
        let goal: Occupant = match room_idx {
            0 => Occupant::A,
            1 => Occupant::B,
            2 => Occupant::C,
            3 => Occupant::D,
            _ => panic!("Rooms must be 0, 1, 2, or 3"),
        };

        self.rooms[room_idx].matches_goal(goal)
    }

    // Returns the number of tiles moved, if it's possible; or else returns None
    fn path_length(&self, start: Position, end: Position) -> Option<u64> {
        match start {
            Position::Hallway(s_hallway) => match end {
                Position::Room(e_room_idx, e_room_pos) => {
                    let e_room = self.rooms[e_room_idx];

                    if (0..=e_room_pos).any(|r| e_room.occupants[r] != Occupant::Empty) {
                        return None;
                    }

                    let room_dist = e_room_pos + 1;
                    let e_hallway = e_room_idx * 2 + 2;

                    let (min_hall, max_hall) = if e_hallway < s_hallway {
                        (e_hallway, s_hallway - 1)
                    } else {
                        (s_hallway + 1, e_hallway)
                    };

                    let hall_dist = max_hall - min_hall + 1;

                    if (min_hall..=max_hall).any(|r| self.hall[r] != Occupant::Empty) {
                        return None;
                    }

                    return Some((room_dist + hall_dist) as u64);
                }
                Position::Hallway(_) => {
                    panic!("I don't think we need this?")
                }
            },
            Position::Room(s_room_idx, s_room_pos) => {
                let start_room = self.rooms[s_room_idx];

                match end {
                    Position::Hallway(e_hallway) => {
                        if (0..s_room_pos)
                            .any(|room_pos| start_room.occupants[room_pos] != Occupant::Empty)
                        {
                            return None;
                        }

                        // distance from your room spot out into the hall
                        let room_dist = s_room_pos + 1;

                        // this is the spot out in the hall; now we need to check the entire hall
                        // is clear
                        let s_hallway = s_room_idx * 2 + 2;
                        let min = e_hallway.min(s_hallway);
                        let max = e_hallway.max(s_hallway);

                        return if (min..=max).all(|r| self.hall[r] == Occupant::Empty) {
                            Some((room_dist + max - min) as u64)
                        } else {
                            None
                        };
                    }
                    Position::Room(_, _) => {
                        panic!("I don't think we need this?")
                    }
                }
            }
        }
    }
}

fn parse(input: &str) -> State<2> {
    fn parse_char(c: Option<char>) -> Occupant {
        match c {
            Some('A') => Occupant::A,
            Some('B') => Occupant::B,
            Some('C') => Occupant::C,
            Some('D') => Occupant::D,
            _ => panic!("bad character {:?}", c),
        }
    }

    let mut out: State<2> = State::default();

    let mut lines = input.lines();

    // making sure the sizes here aren't dynamic because the code expects this exactly
    assert_eq!(lines.next(), Some("#############"));
    assert_eq!(lines.next(), Some("#...........#"));

    // line 3 + 4
    for room_pos in 0..2 {
        let line = lines.next().unwrap();
        let mut line_chars = line.chars();

        if room_pos == 0 {
            assert_eq!(line_chars.next(), Some('#'));
            assert_eq!(line_chars.next(), Some('#'));
        } else {
            assert_eq!(line_chars.next(), Some(' '));
            assert_eq!(line_chars.next(), Some(' '));
        }
        assert_eq!(line_chars.next(), Some('#'));

        for room_idx in 0..4 {
            out.rooms[room_idx].occupants[room_pos] = parse_char(line_chars.next());

            assert_eq!(line_chars.next(), Some('#'));
        }

        if room_pos == 0 {
            assert_eq!(line_chars.next(), Some('#'));
            assert_eq!(line_chars.next(), Some('#'));
        }

        assert_eq!(line_chars.next(), None);
    }

    assert_eq!(lines.next(), Some("  #########"));
    assert_eq!(lines.next(), None);

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::day23::Position::Hallway;

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
            hall: [Empty; HALLWAY_WIDTH],
            rooms: [
                Room { occupants: [B, A] },
                Room { occupants: [C, D] },
                Room { occupants: [B, C] },
                Room { occupants: [D, A] },
            ],
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
    fn path_cost_test_1() {
        let state = parse(SAMPLE);

        let length = state.path_length(Position::Room(2, 0), Hallway(3));

        assert_eq!(length, Some(4));
    }

    #[test]
    fn path_cost_test_1rev() {
        let state = parse(SAMPLE);

        let length = state.path_length(Hallway(3), Position::Room(2, 0));

        assert_eq!(length, None);
    }

    #[test]
    fn path_cost_test_1rev_fixed() {
        let mut state = parse(SAMPLE);

        std::mem::swap(&mut state.hall[3], &mut state.rooms[2].occupants[0]);

        let length = state.path_length(Hallway(3), Position::Room(2, 0));

        assert_eq!(length, Some(4));
    }

    #[test]
    fn path_cost_test_2() {
        let state = parse(SAMPLE);

        let length = state.path_length(Position::Room(0, 0), Hallway(3));

        assert_eq!(length, Some(2));
    }

    #[test]
    fn sample_a_2() {
        // todo: add some unit tests for path cost
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
        let expected = 44169;

        assert_eq!(expected, actual);
    }
}
