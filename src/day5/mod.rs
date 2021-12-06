use std::collections::HashSet;
use std::fs::File;
use std::io::Read;

pub fn a() -> usize {
    a_with_input("./src/day5/full.txt")
}

fn a_with_input(input_path: &str) -> usize {
    let pairs = parse(input_path);

    let mut seen = HashSet::new();
    let mut dupes = HashSet::new();

    for ((x1, y1), (x2, y2)) in pairs.iter().copied() {
        if x1 == x2 {
            let x = x1;
            let ymin = y1.min(y2);
            let ymax = y1.max(y2);

            for y in ymin..(ymax + 1) {
                if !seen.insert((x, y)) {
                    dupes.insert((x, y));
                }
            }
        } else if y1 == y2 {
            let y = y1;
            let xmin = x1.min(x2);
            let xmax = x1.max(x2);

            for x in xmin..(xmax + 1) {
                if !seen.insert((x, y)) {
                    dupes.insert((x, y));
                }
            }
        }

        // else nothing, we skip this for part a
    }

    dupes.len()
}

pub fn b() -> usize {
    b_with_input("./src/day5/full.txt")
}

fn b_with_input(input_path: &str) -> usize {
    let pairs = parse(input_path);

    let mut seen = HashSet::new();
    let mut dupes = HashSet::new();

    for ((x1, y1), (x2, y2)) in pairs.iter().copied() {
        if x1 == x2 {
            let x = x1;
            let ymin = y1.min(y2);
            let ymax = y1.max(y2);

            for y in ymin..(ymax + 1) {
                if !seen.insert((x, y)) {
                    dupes.insert((x, y));
                }
            }
        } else if y1 == y2 {
            let y = y1;
            let xmin = x1.min(x2);
            let xmax = x1.max(x2);

            for x in xmin..(xmax + 1) {
                if !seen.insert((x, y)) {
                    dupes.insert((x, y));
                }
            }
        } else {
            let dx = dir(x1, x2);
            let dy = dir(y1, y2);

            assert_eq!(
                (x2 - x1) * dx,
                (y2 - y1) * dy,
                "Distances should match up: {} -- {} ({}); {} -- {} ({})",
                x1,
                x2,
                dx,
                y1,
                y2,
                dy
            );

            let mut x = x1;
            let mut y = y1;

            let dist = (x2 - x1) * dx;
            assert!(dist > 0);

            for _ in 0..(dist + 1) {
                if !seen.insert((x, y)) {
                    dupes.insert((x, y));
                }
                x += dx;
                y += dy;
            }
        }
    }

    dupes.len()
}

fn dir(a: i64, b: i64) -> i64 {
    if a < b {
        1
    } else if a > b {
        -1
    } else {
        0
    }
}

fn parse(path: &str) -> Vec<((i64, i64), (i64, i64))> {
    let mut buffer = String::new();
    File::open(path)
        .unwrap()
        .read_to_string(&mut buffer)
        .unwrap();

    let comma_pair = |s: &str| {
        let mut toks = s.split(",").map(|t| t.parse::<i64>().unwrap());
        let a = toks.next().unwrap();
        let b = toks.next().unwrap();
        assert!(toks.next().is_none());
        (a, b)
    };

    let mut out = Vec::new();
    for line in buffer.lines() {
        let mut tokens = line.split(" -> ");
        let a = tokens.next().unwrap();
        let b = tokens.next().unwrap();
        assert!(tokens.next().is_none());

        let (a1, a2) = comma_pair(a);
        let (b1, b2) = comma_pair(b);

        out.push(((a1, a2), (b1, b2)));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample_5a() {
        let expected = 5;
        let actual = a_with_input("./src/day5/sample.txt");

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_5b() {
        let expected = 12;
        let actual = b_with_input("./src/day5/sample.txt");

        assert_eq!(expected, actual);
    }
}
