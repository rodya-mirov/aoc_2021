use std::collections::HashMap;

fn input() -> String {
    std::fs::read_to_string("input/input_12.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> usize {
    let pr = parse(input);

    let mut path_count = 0;

    fn recurse(current_index: usize, pr: &ParseResult, visited: &mut [bool], counter: &mut usize) {
        for target_node in pr.edges[current_index].iter().copied() {
            if pr.is_small[target_node] && visited[target_node] {
                continue;
            }

            if target_node == pr.end_ind {
                *counter += 1;
                continue;
            }

            if pr.is_small[target_node] {
                visited[target_node] = true;
                recurse(target_node, pr, visited, counter);
                visited[target_node] = false;
            } else {
                recurse(target_node, pr, visited, counter);
            }
        }
    }

    let mut visited = vec![false; pr.edges.len()];
    visited[pr.start_ind] = true;

    recurse(pr.start_ind, &pr, &mut visited, &mut path_count);

    path_count
}

struct ParseResult {
    // start_ind -> list of end_inds
    // guaranteed to be nonempty for all valid indices
    edges: Vec<Vec<usize>>,
    // guaranteed to be same size as edges
    is_small: Vec<bool>,
    start_ind: usize,
    end_ind: usize,
}

fn parse(input: &str) -> ParseResult {
    let mut namer: HashMap<String, usize> = HashMap::new();

    let mut start_ind = None;
    let mut end_ind = None;
    let mut is_small = Vec::new();

    // (index, is_new)
    let mut assign_ind = |s: String| -> usize {
        if namer.contains_key(&s) {
            return namer.get(&s).copied().unwrap();
        }
        let out = namer.len();

        if &s == "start" {
            assert!(start_ind.is_none());
            start_ind = Some(out);
        } else if &s == "end" {
            assert!(end_ind.is_none());
            end_ind = Some(out);
        }

        fn str_small(s: &str) -> bool {
            s.chars().all(|c| c >= 'a' && c <= 'z')
        }

        is_small.push(str_small(&s));

        namer.insert(s, out);
        out
    };

    let mut edges = Vec::new();

    for line in input.lines() {
        let mut tokens: Vec<String> = line.split('-').map(|s| s.to_string()).collect();
        assert_eq!(tokens.len(), 2);

        let b_ind = assign_ind(tokens.pop().unwrap());
        let a_ind = assign_ind(tokens.pop().unwrap());

        while edges.len() <= a_ind {
            edges.push(Vec::new());
        }
        edges[a_ind].push(b_ind);

        while edges.len() <= b_ind {
            edges.push(Vec::new());
        }
        edges[b_ind].push(a_ind);
    }

    ParseResult {
        edges,
        is_small,
        start_ind: start_ind.unwrap(),
        end_ind: end_ind.unwrap(),
    }
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> usize {
    let pr = parse(input);

    let mut path_count = 0;

    fn recurse(
        current_index: usize,
        pr: &ParseResult,
        visited: &mut [bool],
        has_doubled: bool,
        counter: &mut usize,
    ) {
        for target_node in pr.edges[current_index].iter().copied() {
            if target_node == pr.end_ind {
                *counter += 1;
                continue;
            } else if target_node == pr.start_ind {
                continue;
            }

            if pr.is_small[target_node] {
                if !visited[target_node] {
                    visited[target_node] = true;
                    recurse(target_node, pr, visited, has_doubled, counter);
                    visited[target_node] = false;
                } else if !has_doubled {
                    recurse(target_node, pr, visited, true, counter);
                } else {
                    continue;
                }
            } else {
                recurse(target_node, pr, visited, has_doubled, counter);
            }
        }
    }

    let mut visited = vec![false; pr.edges.len()];
    visited[pr.start_ind] = true;

    recurse(pr.start_ind, &pr, &mut visited, false, &mut path_count);

    path_count
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_A: &'static str = "start-A
start-b
A-c
A-b
b-d
A-end
b-end";

    const SAMPLE_B: &'static str = "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc";

    const SAMPLE_C: &'static str = "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW";

    #[test]
    fn sample_a1() {
        let actual = a_with_input(SAMPLE_A);
        let expected = 10;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a2() {
        let actual = a_with_input(SAMPLE_B);
        let expected = 19;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a3() {
        let actual = a_with_input(SAMPLE_C);
        let expected = 226;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b1() {
        let actual = b_with_input(SAMPLE_A);
        let expected = 36;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b2() {
        let actual = b_with_input(SAMPLE_B);
        let expected = 103;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b3() {
        let actual = b_with_input(SAMPLE_C);
        let expected = 3509;

        assert_eq!(expected, actual);
    }
}
