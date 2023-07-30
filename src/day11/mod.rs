fn input() -> String {
    std::fs::read_to_string("input/input_11.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input::<10, 10>(&contents, 100);

    val.to_string()
}

fn a_with_input<const W: usize, const H: usize>(input: &str, iters: usize) -> u64 {
    let Grid(mut state) = parse::<W, H>(input);

    let mut total_flashes = 0;

    for _step in 0..iters {
        total_flashes += iterate::<W, H>(&mut state)
    }

    total_flashes
}

fn iterate<const W: usize, const H: usize>(state: &mut [[u8; W]; H]) -> u64 {
    let mut total_flashes = 0;
    let mut did_flash = [[false; W]; H];

    let mut to_flash: Vec<(usize, usize)> = Vec::new();

    for y in 0..H {
        for x in 0..W {
            state[y][x] = state[y][x].saturating_add(1);
            if state[y][x] >= 10 {
                to_flash.push((x, y));
            }
        }
    }

    while let Some((x, y)) = to_flash.pop() {
        if did_flash[y][x] {
            continue;
        }

        did_flash[y][x] = true;
        total_flashes += 1;

        let diffs: [(isize, isize); 8] = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];

        for (dx, dy) in diffs {
            let xp = (x as isize) + dx;
            let yp = (y as isize) + dy;
            if xp >= 0 && xp < W as isize && yp >= 0 && yp < H as isize {
                let xp = xp as usize;
                let yp = yp as usize;

                state[yp][xp] = state[yp][xp].saturating_add(1);
                if state[yp][xp] >= 10 {
                    to_flash.push((xp, yp));
                }
            }
        }
    }

    for y in 0..H {
        for x in 0..W {
            if did_flash[y][x] {
                state[y][x] = 0;
            }
        }
    }

    total_flashes
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input::<10, 10>(&contents);

    val.to_string()
}

fn b_with_input<const W: usize, const H: usize>(input: &str) -> u64 {
    let Grid(mut state) = parse::<W, H>(input);

    let mut i = 0;
    loop {
        i += 1;
        let flashes = iterate(&mut state);
        if flashes as usize == W * H {
            return i;
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct Grid<const W: usize, const H: usize>([[u8; W]; H]);

fn parse<const W: usize, const H: usize>(input: &str) -> Grid<W, H> {
    let out: Vec<[u8; W]> = input
        .lines()
        .map(|line| {
            let row: Vec<u8> = line.chars().map(|c| (c as u8) - (b'0')).collect();
            let row: [u8; W] = row.try_into().unwrap();
            row
        })
        .collect();

    let out: [_; H] = out.try_into().unwrap();

    Grid(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526";

    const SMALL_SAMPLE: &'static str = "11111
19991
19191
19991
11111";

    #[test]
    fn sample_a() {
        let actual = a_with_input::<10, 10>(SAMPLE, 100);
        let expected = 1656;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a_small() {
        let actual = a_with_input::<5, 5>(SMALL_SAMPLE, 1);
        let expected = 9;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input::<10, 10>(SAMPLE);
        let expected = 195;

        assert_eq!(expected, actual);
    }
}
