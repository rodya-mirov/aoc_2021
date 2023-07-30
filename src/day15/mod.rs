use std::collections::BinaryHeap;

fn input() -> String {
    std::fs::read_to_string("input/input_15.txt").expect("Should be able to read the file")
}

type CostType = u32;

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> CostType {
    let grid: Vec<Vec<_>> = input
        .lines()
        .map(|line| line.chars().map(|c| (c as CostType) - ('0' as CostType)).collect::<Vec<_>>())
        .collect();

    solve_grid(&grid)
}

fn solve_grid(grid: &Vec<Vec<CostType>>) -> CostType {
    use std::cmp::Reverse;

    let w = grid[0].len();
    let h = grid.len();

    // NOTE: we are assuming it's a square

    #[derive(Ord, PartialEq, Eq, PartialOrd, Debug, Hash, Copy, Clone)]
    struct Seg {
        // needs to be first for the order implementation
        cost_so_far: CostType,
        x: usize,
        y: usize,
    }

    // doing this instead of a HashSet cut my running time by 75%
    struct Seen {
        seen: Vec<Vec<bool>>,
    }

    impl Seen {
        fn new(w: usize, h: usize) -> Self {
            Seen {
                seen: vec![vec![false; w]; h],
            }
        }

        fn insert(&mut self, x: usize, y: usize) -> bool {
            let old = std::mem::replace(&mut self.seen[y][x], true);
            !old
        }
    }

    let mut seen = Seen::new(w, h);
    let mut to_visit: BinaryHeap<Reverse<Seg>> = BinaryHeap::new();

    seen.insert(0, 0);

    to_visit.push(Reverse(Seg {
        x: 0,
        y: 0,
        cost_so_far: 0,
    }));

    while let Some(Reverse(Seg { x, y, cost_so_far })) = to_visit.pop() {
        if x == w - 1 && y == h - 1 {
            return cost_so_far;
        }

        let make_seg = |xp, yp| {
            Reverse(Seg {
                x: xp,
                y: yp,
                cost_so_far: cost_so_far + grid[xp][yp],
            })
        };

        if x > 0 && seen.insert(x - 1, y) {
            to_visit.push(make_seg(x - 1, y));
        }

        if y > 0 && seen.insert(x, y - 1) {
            to_visit.push(make_seg(x, y - 1));
        }

        if x + 1 < w && seen.insert(x + 1, y) {
            to_visit.push(make_seg(x + 1, y));
        }

        if y + 1 < h && seen.insert(x, y + 1) {
            to_visit.push(make_seg(x, y + 1));
        }
    }

    panic!("Solution should have been found")
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> CostType {
    let small_grid: Vec<Vec<_>> = input
        .lines()
        .map(|line| line.chars().map(|c| (c as CostType) - ('0' as CostType)).collect::<Vec<_>>())
        .collect();

    let w = small_grid[0].len();
    let h = small_grid.len();

    let mut large_grid = Vec::with_capacity(h * 5);

    for y in 0..h * 5 {
        let mut row = Vec::with_capacity(w * 5);

        for x in 0..w * 5 {
            let simple_val = small_grid[y % h][x % w];
            let additions = ((x / w) + (y / h)) as CostType;
            let mut val = (simple_val + additions) % 9;
            if val == 0 {
                val = 9;
            }
            row.push(val);
        }

        large_grid.push(row);
    }

    solve_grid(&large_grid)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581";

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 40;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(SAMPLE);
        let expected = 315;

        assert_eq!(expected, actual);
    }
}
