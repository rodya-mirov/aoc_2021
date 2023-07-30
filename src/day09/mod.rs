use std::collections::HashMap;

fn input() -> String {
    std::fs::read_to_string("input/input_09.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> u32 {
    let grid = parse_input(input);

    let mut total = 0;

    for y in 0..grid.len() {
        let row = grid.get(y).unwrap();

        for x in 0..row.len() {
            let val = row[x];

            // turning this big boolean message into a single condition is _possible_ but
            // i don't think it's _helpful_
            #[allow(clippy::if_same_then_else)]
            #[allow(clippy::needless_bool)]
            let is_low = {
                if x > 0 && val >= row[x - 1] {
                    false
                } else if x + 1 < row.len() && val >= row[x + 1] {
                    false
                } else if y > 0 && val >= grid[y - 1][x] {
                    false
                } else if y + 1 < grid.len() && val >= grid[y + 1][x] {
                    false
                } else {
                    true
                }
            };
            if is_low {
                total += val + 1;
            }
        }
    }

    total
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> usize {
    let grid = parse_input(input);

    // basin root location -> number of locations that flow into it
    let mut basin_size: HashMap<(usize, usize), usize> = HashMap::new();

    // "here" to "center of my basin"
    let mut basin_lookup: HashMap<(usize, usize), (usize, usize)> = HashMap::new();

    // note that height 9 is never part of a basin so it's excluded from the loop
    // if there were a lot of possible height values another algorithm would be needed
    // this is O(mnh) where the grid is m by n, and there are h height values
    for height in 0..9 {
        // sweep through the grid
        for y in 0..grid.len() {
            let row = grid.get(y).unwrap();

            for x in 0..row.len() {
                let val = row[x];
                if val != height {
                    continue;
                }

                // this assumes that in all cases you're either
                // (a) a local low point, or
                // (b) there is exactly one adjacent point which is lower than you
                // problem is unspecified otherwise. this does not check for ties, multiple
                // options, etc.
                let basin_root: (usize, usize) = {
                    if x > 0 && val > row[x - 1] {
                        basin_lookup.get(&(x - 1, y)).copied().unwrap()
                    } else if x + 1 < row.len() && val > row[x + 1] {
                        basin_lookup.get(&(x + 1, y)).copied().unwrap()
                    } else if y > 0 && val > grid[y - 1][x] {
                        basin_lookup.get(&(x, y - 1)).copied().unwrap()
                    } else if y + 1 < grid.len() && val > grid[y + 1][x] {
                        basin_lookup.get(&(x, y + 1)).copied().unwrap()
                    } else {
                        (x, y)
                    }
                };

                basin_lookup.insert((x, y), basin_root);
                *basin_size.entry(basin_root).or_insert(0) += 1;
            }
        }
    }

    // println!("Basins: {:#?}", basin_size);

    let mut basin_sizes: Vec<usize> = basin_size.values().copied().collect();
    basin_sizes.sort();
    let len = basin_sizes.len();

    basin_sizes[len - 1] * basin_sizes[len - 2] * basin_sizes[len - 3]
}

fn parse_input(input: &str) -> Vec<Vec<u32>> {
    input
        .lines()
        .map(|line| line.chars().map(|c| (c as u32) - ('0' as u32)).collect::<Vec<u32>>())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str = "2199943210
3987894921
9856789892
8767896789
9899965678";

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 15;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(SAMPLE);
        let expected = 1134;

        assert_eq!(expected, actual);
    }
}
