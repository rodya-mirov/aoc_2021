fn input() -> String {
    std::fs::read_to_string("input/input_07.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> u32 {
    let mut all_pos: Vec<i32> = input.split(',').map(|tok| tok.parse::<i32>().unwrap()).collect();
    all_pos.sort();
    let all_pos = all_pos;

    let min = all_pos[0];
    let max = all_pos[all_pos.len() - 1];

    let mut best = u32::MAX;
    for goal in min..(max + 1) {
        let cost = all_pos.iter().copied().map(|p| p.abs_diff(goal)).sum();
        best = best.min(cost);
    }
    best
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> u32 {
    let mut all_pos: Vec<i32> = input.split(',').map(|tok| tok.parse::<i32>().unwrap()).collect();
    all_pos.sort();
    let all_pos = all_pos;

    let min = all_pos[0];
    let max = all_pos[all_pos.len() - 1];

    fn fuel_cost(dist: u32) -> u32 {
        (dist * (dist + 1)) / 2
    }

    let mut best = u32::MAX;
    for goal in min..(max + 1) {
        let cost = all_pos.iter().copied().map(|p| p.abs_diff(goal)).map(fuel_cost).sum();
        best = best.min(cost);
    }
    best
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str = "16,1,2,0,4,2,7,1,2,14";

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 37;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(SAMPLE);
        let expected = 168;

        assert_eq!(expected, actual);
    }
}
