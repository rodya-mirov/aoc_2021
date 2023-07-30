use std::collections::HashSet;

use exec::{BoundedSV, prune_conditions};
use parse::parse;

mod bounds;
mod cmd;
mod exec;
mod parse;

fn input() -> String {
    std::fs::read_to_string("input/input_24.txt").expect("Should be able to read the file")
}

// parse commands, then execute symbolically, tracking bounds as we go
pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> String {
    let cmds = parse(input);

    // complete iterations to process
    let iters = 14;
    // additional steps to walk through (0 means just do the complete iterations)
    let offset = 0;
    // verified by hand through 11+4
    let slice = &cmds[..(18 * iters) + offset];

    let final_state = exec::symbolic_execute(slice);

    let mut all_conditions = exec::collect_conditions(&final_state[3]);

    println!("Identified {} unique conditions", all_conditions.len());

    for (i, c) in all_conditions.iter().enumerate() {
        println!("  {}: {}", i, c.to_string(4));
    }

    // now we basically need to identify collections of conditions where a solution is even
    // possible; then solve
    type Answer = (BoundedSV, Vec<BoundedSV>, Vec<BoundedSV>);

    fn search(
        start: BoundedSV,
        to_be_eval: &mut Vec<BoundedSV>,
        known_true: &mut Vec<BoundedSV>,
        known_false: &mut Vec<BoundedSV>,
        answers: &mut Vec<Answer>,
    ) {
        if to_be_eval.is_empty() {
            if bounds::contains(start.bounds, 0) {
                answers.push((start, known_true.clone(), known_false.clone()));
            }
            return;
        }

        known_true.push(to_be_eval.pop().unwrap());

        let new_state = prune_conditions(start.clone(), known_true, known_false);
        if bounds::contains(new_state.bounds, 0) {
            search(new_state, to_be_eval, known_true, known_false, answers);
        }

        known_false.push(known_true.pop().unwrap());

        let new_state = prune_conditions(start.clone(), known_true, known_false);
        if bounds::contains(new_state.bounds, 0) {
            search(new_state, to_be_eval, known_true, known_false, answers);
        }

        to_be_eval.push(known_false.pop().unwrap());
    }

    let mut solutions = Vec::new();
    search(
        final_state[3].clone(),
        &mut all_conditions,
        &mut Vec::new(),
        &mut Vec::new(),
        &mut solutions,
    );

    println!("Found a total of {} unique solution sets", solutions.len());

    let mut unique_exprs = HashSet::new();
    for (ans, _, _) in solutions.iter() {
        unique_exprs.insert(ans.clone());
    }

    assert_eq!(unique_exprs.len(), 1, "Should be exactly one possible answer expression");

    let answer_expression = unique_exprs.into_iter().next().unwrap();

    println!("Only answer expression is {:?}", answer_expression);

    // next, check which conditions MUST be true and which MUST be false
    let mut always_true = Vec::new();
    let mut always_false = Vec::new();

    for condition in all_conditions.iter() {
        let mut seen_true = false;
        let mut seen_false = false;
        for (_, known_true, known_false) in solutions.iter() {
            if known_true.contains(condition) {
                seen_true = true;
            } else if known_false.contains(condition) {
                seen_false = true;
            } else {
                // should have good coverage honestly
                unreachable!()
            }
        }

        if seen_true && seen_false {
            // no-op
        } else if seen_true {
            println!("Condition {:?} must always be true", condition);
            always_true.push(condition.clone());
        } else if seen_false {
            println!("Condition {:?} must always be false", condition);
            always_false.push(condition.clone());
        } else {
            unreachable!("Every condition should have been checked")
        }
    }

    assert_eq!(always_false.len(), 0, "We didn't get any 'always false' conditions");

    unimplemented!()
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> u64 {
    let _ = parse(input);
    unimplemented!()
}
