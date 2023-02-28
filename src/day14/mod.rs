use std::collections::HashMap;

fn input() -> String {
    std::fs::read_to_string("input/input_14.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents, 10);

    val.to_string()
}

fn a_with_input(input: &str, steps: usize) -> usize {
    let (mut state, rules) = parse(input);

    for _ in 0..steps {
        state = iterate(&state, &rules);
    }

    let mut counts = HashMap::new();

    for (p, mult) in state.pairs.into_iter() {
        *counts.entry(p[0]).or_insert(0) += mult;
        *counts.entry(p[1]).or_insert(0) += mult;
    }

    for v in counts.values_mut() {
        *v /= 2;
    }

    *counts.entry(state.start).or_insert(0) += 1;
    *counts.entry(state.end).or_insert(0) += 1;

    let mut min_count = usize::MAX;
    let mut max_count = usize::MIN;

    for v in counts.values().copied() {
        min_count = min_count.min(v);
        max_count = max_count.max(v);
    }

    max_count - min_count
}

fn iterate(state: &State, rules: &HashMap<Pair, [Pair; 2]>) -> State {
    let mut out = HashMap::new();

    for (pair, mult) in state.pairs.iter().map(|(pair, mult)| (*pair, *mult)) {
        if let Some([a, b]) = rules.get(&pair).copied() {
            *out.entry(a).or_insert(0) += mult;
            *out.entry(b).or_insert(0) += mult;
        } else {
            *out.entry(pair).or_insert(0) += mult;
        }
    }

    State {
        start: state.start,
        end: state.end,
        pairs: out,
    }
}

type Pair = [char; 2];

#[derive(Debug)]
struct State {
    start: char,
    end: char,
    pairs: HashMap<Pair, usize>,
}

fn parse(input: &str) -> (State, HashMap<Pair, [Pair; 2]>) {
    let mut lines = input.lines();

    let state = {
        let template = lines.next().unwrap().to_string();

        assert!(template.is_ascii());
        assert!(template.len() > 1);

        let mut chars = template.chars();

        let start = chars.next().unwrap();
        let mut last = start;
        let mut pairs = HashMap::new();

        for c in chars {
            let me = [last, c];

            *pairs.entry(me).or_default() += 1;

            last = c;
        }

        let end = last;

        State { start, end, pairs }
    };

    lines.next(); // skip empty line

    let mut rules = HashMap::new();

    for line in lines {
        let mut tokens: Vec<String> = line.split(" -> ").map(|s| s.to_string()).collect();
        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0].len(), 2);
        assert_eq!(tokens[1].len(), 1);

        let b: char = tokens.pop().unwrap().chars().next().unwrap();

        let a: Vec<char> = tokens.pop().unwrap().chars().collect();
        let a: [char; 2] = a.try_into().unwrap();

        rules.insert(a, [[a[0], b], [b, a[1]]]);
    }

    (state, rules)
}

pub fn b() -> String {
    let contents = input();

    a_with_input(&contents, 40).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C";

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE, 10);
        let expected = 1588;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = a_with_input(SAMPLE, 40);
        let expected = 2188189693529;

        assert_eq!(expected, actual);
    }
}
