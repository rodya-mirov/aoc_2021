use std::collections::HashSet;

fn input() -> String {
    std::fs::read_to_string("input/input_13.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> usize {
    let ParseResult { mut dots, folds } = parse(input);

    // only do the one fold
    let fold = folds[0];

    dots = apply(dots, fold);

    dots.len()
}

fn apply(mut dots: Vec<(i32, i32)>, fold: Fold) -> Vec<(i32, i32)> {
    if dots.is_empty() {
        return dots;
    }

    let mut out = Vec::with_capacity(dots.len());

    for (x, y) in dots.drain(..) {
        out.push(match fold {
            Fold::FoldAlongX(axis) => {
                if x < axis {
                    (x, y)
                } else if x > axis {
                    (axis - (x - axis), y)
                } else {
                    unimplemented!("Can't have x=axis")
                }
            }
            Fold::FoldAlongY(axis) => {
                if y < axis {
                    (x, y)
                } else if y > axis {
                    (x, axis - (y - axis))
                } else {
                    unimplemented!("can't have y=axis")
                }
            }
        });
    }

    out.sort();

    // just reusing the allocation, probably unnecessary
    let mut deduped = dots;

    let mut last = None;
    for pt in out.into_iter().rev() {
        if Some(pt) != last {
            last = Some(pt);
            deduped.push(pt);
        }
    }

    deduped
}

pub fn b() -> String {
    let contents = input();

    b_with_input(&contents)
}

fn b_with_input(input: &str) -> String {
    let ParseResult { mut dots, folds } = parse(input);

    // only do the one fold
    for fold in folds {
        dots = apply(dots, fold);
    }

    let mut x_min = i32::MAX;
    let mut x_max = i32::MIN;
    let mut y_min = i32::MAX;
    let mut y_max = i32::MIN;

    for (x, y) in dots.iter().copied() {
        x_max = x_max.max(x);
        x_min = x_min.min(x);

        y_max = y_max.max(y);
        y_min = y_min.min(y);
    }

    let dots: HashSet<(i32, i32)> = dots.into_iter().collect();

    let mut out = String::new();

    for y in y_min..=y_max {
        if !out.is_empty() {
            out.push('\n');
        }

        for x in x_min..=x_max {
            if dots.contains(&(x, y)) {
                out.push('#');
            } else {
                out.push('.');
            }
        }
    }

    out
}

struct ParseResult {
    dots: Vec<(i32, i32)>,
    folds: Vec<Fold>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
enum Fold {
    FoldAlongX(i32),
    FoldAlongY(i32),
}

fn parse(input: &str) -> ParseResult {
    let mut dots = Vec::new();
    let mut folds = Vec::new();

    let mut lines = input.lines();

    let mut next = lines.next().unwrap();

    while !next.is_empty() {
        let point: Vec<i32> = next.split(',').map(|s| s.parse::<i32>().unwrap()).collect();
        assert_eq!(point.len(), 2);
        dots.push((point[0], point[1]));

        next = lines.next().unwrap();
    }

    for next in lines {
        let tokens: Vec<_> = next.split('=').collect();

        assert_eq!(tokens.len(), 2);

        let axis = tokens[1].parse::<i32>().unwrap();

        let fold = match tokens[0] {
            "fold along y" => Fold::FoldAlongY(axis),
            "fold along x" => Fold::FoldAlongX(axis),
            _ => unreachable!("Bad input: {}", next),
        };

        folds.push(fold);
    }

    ParseResult { dots, folds }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5";

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 17;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(SAMPLE);
        let expected = "#####
#...#
#...#
#...#
#####"
            .to_string();

        assert_eq!(expected, actual);
    }
}
