use std::fmt;

fn input() -> String {
    std::fs::read_to_string("input/input_24.txt").expect("Should be able to read the file")
}

type Int = i64;

// there are only 4 variables, so a 'pointer' only needs 2 bits, but this is as small as I can go
type Variable = u8;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Ord, PartialOrd)]
enum Value {
    Variable(Variable),
    Integer(Int),
}

impl Value {
    fn mentions(self, var: Variable) -> bool {
        match self {
            Value::Integer(_) => false,
            Value::Variable(v) => v == var,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
enum Command {
    // TODO: prefer sorting by modified variable first, then by content, instead
    //         of "inputs last" which can obscure some optimizations
    TwoArg(Variable, TwoArgOperand, Value),
    OneArg(Variable, OneArgOperand),
    Inp(Variable),
}

// Note we rely on the derived ordering (two independent instructions will be sorted according to it)
// so don't reorder these
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
enum TwoArgOperand {
    Set,
    Add,
    Mul,
    Div,
    Mod,
    Eql,
    Neq,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
enum OneArgOperand {
    // literally just "set val to val % 26;" for whatever reason every mod is mod 26
    Mod26,
}

impl Command {
    // returns the variable which is modified by this command
    fn modified_var(self) -> Variable {
        use Command::*;

        match self {
            TwoArg(v, _, _) => v,
            OneArg(v, _) => v,
            Inp(v) => v,
        }
    }

    // determines whether the command actually reads (that is, depends on the start value of)
    // the given variable.
    fn reads(self, goal_var: Variable) -> bool {
        match self {
            Command::Inp(_) => false,
            Command::OneArg(v, OneArgOperand::Mod26) => v == goal_var,
            Command::TwoArg(var, op, val) => {
                if val.mentions(goal_var) {
                    true
                } else if var != goal_var {
                    false
                } else {
                    match op {
                        // this is the only operand that doesn't depend on its LHS
                        TwoArgOperand::Set => false,
                        _ => true,
                    }
                }
            }
        }
    }

    fn mentions(self, goal_var: Variable) -> bool {
        use Command::*;

        match self {
            TwoArg(var, _, val) => var == goal_var || val.mentions(goal_var),
            OneArg(var, _) => var == goal_var,
            Inp(var) => var == goal_var,
        }
    }
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> String {
    let mut cmds = parse(input);

    if false {
        let inps = 5;

        let [_, _, _, z_state] = symbolic_execute(&cmds[..(18 * inps)], &[]);
        println!(
            "After {} inputs, got state {:#?} and bounds {:?}",
            inps, z_state, z_state.bounds
        );
    }

    optimize(&mut cmds);

    let mut digits = Vec::new();

    let soln = recursive_search_a(&cmds, &mut digits);

    let Some(soln) = soln else { panic!("No solution found")};

    soln.iter().copied().map(|i| i.to_string()).collect()
}

fn possible_success(bounds: Bounds) -> bool {
    if let Some(lb) = bounds.0 {
        if 0 < lb {
            return false;
        }
    }

    if let Some(ub) = bounds.1 {
        if ub < 0 {
            return false;
        }
    }

    true
}

fn recursive_search_a(cmds: &[Command], digits_so_far: &mut Vec<Int>) -> Option<Vec<Int>> {
    if digits_so_far.len() <= 7 {
        println!("Diving into digits {:?} ...", digits_so_far);
    }

    for digit in (1..10).rev() {
        digits_so_far.push(digit);
        let state = symbolic_execute(cmds, digits_so_far);

        if &state[3].sv == &SymbolicValue::Integer(0) && digits_so_far.len() == 14 {
            return Some(digits_so_far.clone());
        }

        if possible_success(state[3].bounds) {
            if let Some(sub_result) = recursive_search_a(cmds, digits_so_far) {
                return Some(sub_result);
            }
        } else if digits_so_far.len() < 13 {
            // this is just a debug log; this indicates that we actually got to skip a recursion
            // because of the bounding
            println!(
                "Digits (len {}) {:?} rejected because of bounds {:?}",
                digits_so_far.len(),
                digits_so_far,
                state[3].bounds
            );
        }

        digits_so_far.pop();
    }

    // no success possible :(
    None
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

fn parse_variable(input: &str) -> Result<Variable, &str> {
    match input {
        "w" => Ok(0),
        "x" => Ok(1),
        "y" => Ok(2),
        "z" => Ok(3),
        _ => Err(input),
    }
}

fn parse_value(input: &str) -> Value {
    if let Ok(v) = parse_variable(input) {
        Value::Variable(v)
    } else {
        Value::Integer(input.parse().unwrap())
    }
}

fn parse_line(input: &str) -> Command {
    let tokens: Vec<&str> = input.split_ascii_whitespace().collect();

    match tokens.len() {
        2 => {
            let var = parse_variable(tokens[1]).unwrap();

            match tokens[0] {
                "inp" => Command::Inp(var),
                other => panic!("Unrecognized one-arg operation {}", other),
            }
        }
        3 => {
            let var = parse_variable(tokens[1]).unwrap();
            let val = parse_value(tokens[2]);

            let op: TwoArgOperand = match tokens[0] {
                "add" => TwoArgOperand::Add,
                "mul" => TwoArgOperand::Mul,
                "div" => TwoArgOperand::Div,
                "mod" => TwoArgOperand::Mod,
                "eql" => TwoArgOperand::Eql,
                other => panic!("Unrecognized two-arg operation {}", other),
            };

            if op == TwoArgOperand::Mod && val == Value::Integer(26) {
                Command::OneArg(var, OneArgOperand::Mod26)
            } else if op == TwoArgOperand::Mul && val == Value::Integer(0) {
                Command::TwoArg(var, TwoArgOperand::Set, Value::Integer(0))
            } else {
                Command::TwoArg(var, op, val)
            }
        }
        other => panic!(
            "Should never get exactly {} tokens in a line (line was {})",
            other, input
        ),
    }
}

fn parse(input: &str) -> Vec<Command> {
    input.lines().map(|line| parse_line(line)).collect()
}

// I0 is first read, I1 is next, etc. So the index is just the "read number"
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
struct InputValue(usize);

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
enum SymbolicValue {
    DerivedOne(OneArgOperand, Box<BoundedSV>),
    // might want to end up doing Rc or something here but not right this second
    // but there are a LOT of clones, idk if it will end up mattering
    Derived(Box<BoundedSV>, TwoArgOperand, Box<BoundedSV>),
    // cond, val_if_true, val_if_false
    Ternary(Box<BoundedSV>, Box<BoundedSV>, Box<BoundedSV>),
    Input(InputValue),
    Integer(Int),
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct BoundedSV {
    sv: SymbolicValue,
    bounds: Bounds,
}

impl BoundedSV {
    fn is_constant(&self) -> bool {
        self.sv.is_constant()
    }
}

impl fmt::Debug for BoundedSV {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.sv)
    }
}

impl fmt::Debug for SymbolicValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolicValue::Integer(i) => write!(f, "{}", i),
            SymbolicValue::Input(InputValue(ind)) => write!(f, "W{}", ind),
            SymbolicValue::DerivedOne(OneArgOperand::Mod26, a) => {
                write!(f, "({:?}", a)?;
                write!(f, " % 26)")?;
                Ok(())
            }
            SymbolicValue::Ternary(cond, if_true, if_false) => {
                write!(
                    f,
                    "IF ({:?}) THEN ({:?}) ELSE ({:?})",
                    cond, if_true, if_false
                )
            }
            SymbolicValue::Derived(a, op, b) => {
                write!(f, "({:?}", a)?;
                write!(
                    f,
                    " {} ",
                    match op {
                        TwoArgOperand::Set => "set to",
                        TwoArgOperand::Add => "+",
                        TwoArgOperand::Mul => "*",
                        TwoArgOperand::Div => "/",
                        TwoArgOperand::Mod => "%",
                        TwoArgOperand::Eql => "==",
                        TwoArgOperand::Neq => "!=",
                    }
                )?;
                write!(f, "{:?})", b)?;
                Ok(())
            }
        }
    }
}

impl SymbolicValue {
    fn is_constant(&self) -> bool {
        match self {
            SymbolicValue::Integer(_) => true,
            SymbolicValue::Input(_) => false,
            SymbolicValue::DerivedOne(_, a) => a.is_constant(),
            SymbolicValue::Derived(a, _, b) => a.is_constant() && b.is_constant(),
            SymbolicValue::Ternary(a, b, c) => {
                a.is_constant() && b.is_constant() && c.is_constant()
            }
        }
    }
}

type Bounds = (Option<Int>, Option<Int>);

fn to_const(a: Bounds) -> Option<Int> {
    if a.0.is_some() && a.0 == a.1 {
        a.0
    } else {
        None
    }
}

fn are_disjoint(a: Bounds, b: Bounds) -> bool {
    // just a huge number of cases, idk, it's a lot
    if let ((_, Some(aub)), (Some(blb), _)) = (a, b) {
        if aub < blb {
            return true;
        }
    }
    if let ((Some(alb), _), (_, Some(bub))) = (a, b) {
        if bub < alb {
            return true;
        }
    }

    false
}

fn is_everything(b: Bounds) -> bool {
    b.0.is_none() && b.1.is_none()
}

/// Returns the smallest Bounds object which contains the two given Bounds objects
fn union(a: Bounds, b: Bounds) -> Bounds {
    let lb = match (a.0, b.0) {
        (None, _) => None,
        (_, None) => None,
        (Some(a), Some(b)) => Some(a.min(b)),
    };
    let ub = match (a.1, b.1) {
        (None, _) => None,
        (_, None) => None,
        (Some(a), Some(b)) => Some(a.max(b)),
    };
    (lb, ub)
}

fn safe_union(a: Option<Bounds>, b: Option<Bounds>) -> Option<Bounds> {
    match (a, b) {
        (None, None) => None,
        (Some(a), None) => Some(a),
        (None, Some(b)) => Some(b),
        (Some(a), Some(b)) => Some(union(a, b)),
    }
}

fn to_nonneg(b: Bounds) -> Option<Bounds> {
    let (mut lb, ub) = b;

    if let Some(ub_val) = ub {
        if ub_val < 0 {
            return None;
        }
    }

    lb = match lb {
        None => Some(0),
        Some(val) if val < 0 => Some(0),
        Some(val) => Some(val),
    };

    Some((lb, ub))
}

fn to_neg(b: Bounds) -> Option<Bounds> {
    let (lb, mut ub) = b;

    if let Some(lb_val) = lb {
        if lb_val >= 0 {
            return None;
        }
    }

    ub = match ub {
        None => Some(-1),
        Some(val) if val > -1 => Some(-1),
        Some(val) => Some(val),
    };

    Some((lb, ub))
}

/// Negates the bounds, properly flipping; so (-1, 3) becomes (-3, 1) and (2, inf) becomes (-inf, -2)
fn negate(b: Bounds) -> Bounds {
    let (lb, ub) = b;
    (ub.map(|i| -i), lb.map(|i| -i))
}

fn const_bounds(i: Int) -> Bounds {
    (Some(i), Some(i))
}

fn is_zero(bounds: Bounds) -> bool {
    bounds == (Some(0), Some(0))
}

/// None + _ or _ + None is None
/// Otherwise add the values; overflow gets None again
fn safe_add(a: Option<Int>, b: Option<Int>) -> Option<Int> {
    match (a, b) {
        (Some(a), Some(b)) => a.checked_add(b),
        _ => None,
    }
}

fn mult_bounds(a: Bounds, b: Bounds) -> Bounds {
    if is_zero(a) || is_zero(b) {
        const_bounds(0)
    } else if is_everything(a) || is_everything(b) {
        (None, None)
    } else {
        let a_neg_range = match to_neg(a) {
            None => None,
            Some(neg_a) => Some(negate(mult_bounds(negate(neg_a), b))),
        };

        let mut running_range = a_neg_range;

        let a_nonneg = to_nonneg(a);
        if a_nonneg.is_none() {
            return a_neg_range.expect("They can't both be empty, I think");
        }

        let a_nonneg = a_nonneg.unwrap();

        let b_neg_range = match to_neg(b) {
            None => None,
            Some(neg_b) => Some(negate(mult_bounds(a_nonneg, negate(neg_b)))),
        };

        running_range = safe_union(running_range, b_neg_range);

        let b_nonneg = to_nonneg(b);
        if b_nonneg.is_none() {
            return running_range.expect("I feel like we must have found something");
        }

        // now we can just consider the nonnegative parts of both, which is much easier
        let (Some(a_lb), a_ub) = a else { unreachable!("We ensured bounds >= 0")};
        let (Some(b_lb), b_ub) = b else { unreachable!("We ensured b bounds >= 0")};

        let lb = a_lb
            .checked_mul(b_lb)
            .expect("Bounded mul overflow oh noooo");

        let ub = a_ub.and_then(|a_ub_val| b_ub.and_then(|b_ub_val| a_ub_val.checked_mul(b_ub_val)));

        let nonneg_part: Option<Bounds> = Some((Some(lb), ub));

        safe_union(nonneg_part, running_range).expect("At least one part is nontrivial")
    }
}

fn div_bounds(lhs: Bounds, rhs: Bounds) -> Bounds {
    let wide_bounds = union(const_bounds(0), lhs);

    // TODO: we can actually do better than this if rhs is bounded away from zero but I don't
    //          want to deal with it right now
    if Some(rhs) == to_nonneg(rhs) {
        wide_bounds
    } else {
        union(wide_bounds, negate(wide_bounds))
    }
}

fn is_mul_26(a: &SymbolicValue) -> bool {
    match a {
        SymbolicValue::Derived(_, TwoArgOperand::Mul, other) => match other.as_ref() {
            BoundedSV {
                sv: SymbolicValue::Integer(26),
                ..
            } => true,
            _ => false,
        },
        _ => false,
    }
}

fn const_sv(val: Int) -> BoundedSV {
    BoundedSV {
        sv: SymbolicValue::Integer(val),
        bounds: const_bounds(val),
    }
}

// return the final state (symbolically) as well as the number of reads
fn symbolic_execute(cmds: &[Command], fixed_inps: &[Int]) -> [BoundedSV; 4] {
    let mut state = [const_sv(0), const_sv(0), const_sv(0), const_sv(0)];

    let mut read_no: usize = 0;

    fn eval(state: &[BoundedSV], val: Value) -> BoundedSV {
        match val {
            Value::Integer(i) => const_sv(i),
            Value::Variable(v) => state[v as usize].clone(),
        }
    }

    for &cmd in cmds {
        match cmd {
            Command::Inp(var) => {
                if read_no < fixed_inps.len() {
                    let val = fixed_inps[read_no];
                    state[var as usize] = const_sv(val);
                } else {
                    state[var as usize] = BoundedSV {
                        sv: SymbolicValue::Input(InputValue(read_no)),
                        bounds: (Some(1), Some(9)),
                    };
                }
                read_no += 1;
            }
            Command::OneArg(var, op) => {
                let lhs = state[var as usize].clone();
                let new_val = eval_one_arg(lhs, op);

                state[var as usize] = simplify_val(new_val);
            }
            Command::TwoArg(var, op, val) => {
                let lhs = state[var as usize].clone();
                let rhs = eval(&state, val);

                let new_val = eval_two_arg(lhs, op, rhs);

                state[var as usize] = simplify_val(new_val);
            }
        }
    }

    state
}

fn simplify_val(bsv: BoundedSV) -> BoundedSV {
    if let Some(val) = to_const(bsv.bounds) {
        if !bsv.sv.is_constant() {
            println!("Oh yes! Got a nontrivial const bound! from {:?} ({:?}) to {:?}", bsv.sv, bsv.bounds, val);
        }
        const_sv(val)
    } else {
        bsv
    }
}

/// Pushes the mapper inside the ternary, if matches
fn handle_ternary<F: FnMut(BoundedSV) -> BoundedSV>(lhs: BoundedSV, mut mapper: F) -> BoundedSV {
    match lhs {
        BoundedSV { sv: SymbolicValue::Ternary(cond, if_true, if_false), bounds: _} => {
            let new_if_true = mapper(*if_true);
            let new_if_false = mapper(*if_false);
            let new_bounds = union(new_if_true.bounds, new_if_false.bounds);
            BoundedSV {
                sv: SymbolicValue::Ternary(cond, Box::new(new_if_true), Box::new(new_if_false)),
                bounds: new_bounds
            }
        },
        non_ternary => mapper(non_ternary)
    }
}

fn eval_one_arg(lhs: BoundedSV, op: OneArgOperand) -> BoundedSV {
    if lhs.is_constant() {
        let SymbolicValue::Integer(lhs) = lhs.sv else { unreachable!()};
        let new_val = (lhs) % 26;
        const_sv(new_val)
    } else {
        match op {
            OneArgOperand::Mod26 => {
                match lhs.bounds {
                    // if we know the lhs < 26, then the mod is a no-op and bounds don't change
                    (_, Some(ub)) if ub < 26 => lhs,
                    // otherwise the bounds get all screwy and we just say it's between 0 and 25
                    _ => {
                        handle_ternary(lhs, |lhs| {
                            match lhs {
                                // this looks real specific, but we exploit the structure we have
                                // basically this is (a*26+b)%26 -> b%26
                                BoundedSV {
                                    bounds: _,
                                    sv: SymbolicValue::Derived(a, TwoArgOperand::Add, b),
                                } if is_mul_26(&a.as_ref().sv) => {
                                    // TODO: we can actually improve the bounds from here
                                    BoundedSV {
                                        bounds: (Some(0), Some(25)),
                                        sv: SymbolicValue::DerivedOne(OneArgOperand::Mod26, b)
                                    }
                                }
                                lhs => BoundedSV {
                                    bounds: (Some(0), Some(25)),
                                    sv: SymbolicValue::DerivedOne(
                                        OneArgOperand::Mod26,
                                        Box::new(lhs),
                                    ),
                                },
                            }
                        })
                    }
                }
            }
        }
    }
}

fn eval_two_arg(lhs: BoundedSV, op: TwoArgOperand, rhs: BoundedSV) -> BoundedSV {
    let lhs_bounds = lhs.bounds;
    let rhs_bounds = rhs.bounds;

    // buncha very-specific optimizations based on what i saw wasting time
    match op {
        TwoArgOperand::Set => rhs,
        TwoArgOperand::Add => match (lhs, rhs) {
            (BoundedSV { sv: SymbolicValue::Integer(a), .. }, BoundedSV { sv: SymbolicValue::Integer(b), ..}) => {
                let val = a.checked_add(b).expect("Addition overflow oh noooo");
                const_sv(val)
            }
            (BoundedSV { sv: SymbolicValue::Integer(0), ..}, rhs) => rhs,
            (lhs, BoundedSV { sv: SymbolicValue::Integer(0), ..}) => lhs,
            (lhs, rhs) => {
                handle_ternary(lhs, |lhs| {
                    let new_bounds = (
                        safe_add(lhs_bounds.0, rhs_bounds.0),
                        safe_add(lhs_bounds.1, rhs_bounds.1),
                    );
                    let new_val =
                        SymbolicValue::Derived(Box::new(lhs.clone()), op, Box::new(rhs.clone()));
                    BoundedSV {
                        bounds: new_bounds,
                        sv: new_val
                    }
                })
            }
        },
        TwoArgOperand::Mul => {
            match (lhs, rhs) {
                (BoundedSV { sv: SymbolicValue::Integer(1), ..}, rhs) => rhs,
                (lhs, BoundedSV { sv: SymbolicValue::Integer(1), ..}) => lhs,
                (BoundedSV { sv: SymbolicValue::Integer(0), ..}, _) => const_sv(0),
                (_, BoundedSV { sv: SymbolicValue::Integer(0), ..}) => const_sv(0),
                (lhs ,rhs) => {
                    if lhs.is_constant() && rhs.is_constant() {
                        let SymbolicValue::Integer(lhs) = lhs.sv else { unreachable!() };
                        let SymbolicValue::Integer(rhs) = rhs.sv else { unreachable!("Should have collapsed constant: {:?}", rhs) };
                        let new_val =
                            (lhs).checked_mul(rhs).expect("Mult overflow oh nooo");
                        const_sv(new_val)
                    } else {
                        let new_bounds = mult_bounds(lhs_bounds, rhs_bounds);
                        let new_val = SymbolicValue::Derived(
                            Box::new(lhs.clone()),
                            op,
                            Box::new(rhs),
                        );
                        BoundedSV {
                            bounds: new_bounds,
                            sv: new_val
                        }
                    }
                }
            }
        }
        TwoArgOperand::Mod => {
            unreachable!("Mod has all been optimized away, I think");
        }
        TwoArgOperand::Div => {
            match (lhs, rhs) {
                (BoundedSV{sv: SymbolicValue::Integer(0), ..}, _) => const_sv(0),
                (lhs, BoundedSV{sv: SymbolicValue::Integer(1), ..}) => lhs,
                (lhs, rhs) => {
                    if lhs.is_constant() && rhs.is_constant() {
                        let SymbolicValue::Integer(lhs) = lhs.sv else { unreachable!() };
                        let SymbolicValue::Integer(rhs) = rhs.sv else { unreachable!() };
                        if rhs <= 0 || lhs < 0 {
                            panic!("BAD DIV ARGS: {} {}", lhs, rhs);
                        }
                        let new_val = lhs / rhs;
                        const_sv(new_val)
                    } else {
                        let new_bounds = div_bounds(lhs_bounds, rhs_bounds);

                        let new_val = SymbolicValue::Derived(
                            Box::new(lhs.clone()),
                            op,
                            Box::new(rhs),
                        );

                        BoundedSV {
                            bounds: new_bounds,
                            sv: new_val
                        }
                    }
                }
            }
        }
        TwoArgOperand::Eql => {
            unimplemented!("Equality should all be optimized away");
        }
        TwoArgOperand::Neq => {
            if lhs.is_constant() && rhs.is_constant() {
                let SymbolicValue::Integer(lhs) = lhs.sv else { unreachable!()};
                let SymbolicValue::Integer(rhs) = rhs.sv else {unreachable!()};
                let new_val = if lhs != rhs { 1 } else { 0 };
                const_sv(new_val)
            } else if are_disjoint(lhs_bounds, rhs_bounds) {
                const_sv(1)
            } else {
                // println!("SAD SAD SAD: Could not resolve inequality: {:?} vs {:?}", lhs, rhs);
                let new_val = BoundedSV {
                    bounds: (Some(0), Some(1)),
                    sv: SymbolicValue::Derived(
                        Box::new(lhs.clone()),
                        TwoArgOperand::Neq,
                        Box::new(rhs),
                    )
                };
                let new_val = SymbolicValue::Ternary(
                    Box::new(new_val),
                    Box::new(const_sv(1)),
                    Box::new(const_sv(0)),
                );
                BoundedSV {
                    bounds: (Some(0), Some(1)),
                    sv: new_val
                }
            }
        }
    }
}

// Two commands are independent if it doesn't matter which order you execute them in
fn is_independent(a: Command, b: Command) -> bool {
    let a_modifies = a.modified_var();
    if b.mentions(a_modifies) {
        return false;
    }

    let b_modifies = b.modified_var();
    if a.mentions(b_modifies) {
        return false;
    }

    true
}

fn maybe_simplify(a: Command) -> Option<Command> {
    if let Command::TwoArg(var, TwoArgOperand::Mul, Value::Integer(0)) = a {
        return Some(Command::TwoArg(var, TwoArgOperand::Set, Value::Integer(0)));
    }

    if let Command::TwoArg(var, TwoArgOperand::Mod, Value::Integer(26)) = a {
        return Some(Command::OneArg(var, OneArgOperand::Mod26));
    }

    None
}

fn is_noop(a: Command) -> bool {
    if let Command::TwoArg(_, TwoArgOperand::Add, Value::Integer(0)) = a {
        true
    } else if let Command::TwoArg(_, TwoArgOperand::Mul, Value::Integer(1)) = a {
        true
    } else if let Command::TwoArg(_, TwoArgOperand::Div, Value::Integer(1)) = a {
        true
    } else {
        false
    }
}

fn maybe_combine(a: Command, b: Command) -> Option<Command> {
    if is_noop(b) {
        return Some(a);
    } else if is_noop(a) {
        return Some(b);
    }

    let am = a.modified_var();
    let bm = b.modified_var();

    // first thing is, if we simply set the same variable twice, collapse to the second one
    if am == bm && !b.reads(am) {
        return Some(b);
    }

    // another very specific thing we seem to see a lot
    if am == bm {
        if let Command::TwoArg(_, TwoArgOperand::Eql, val) = a {
            if let Command::TwoArg(_, TwoArgOperand::Eql, Value::Integer(0)) = b {
                return Some(Command::TwoArg(am, TwoArgOperand::Neq, val));
            }
        }
    }

    None
}

fn optimize(cmds: &mut Vec<Command>) {
    // first, make it explicit that we know the start values
    cmds.insert(0, Command::TwoArg(0, TwoArgOperand::Set, Value::Integer(0)));
    cmds.insert(0, Command::TwoArg(1, TwoArgOperand::Set, Value::Integer(0)));
    cmds.insert(0, Command::TwoArg(2, TwoArgOperand::Set, Value::Integer(0)));
    cmds.insert(0, Command::TwoArg(3, TwoArgOperand::Set, Value::Integer(0)));

    // We sort of swap / combine as we go; keep iterating through until the process stabilizes
    loop {
        let mut made_change = false;

        let mut i = 0;
        while i + 1 < cmds.len() {
            let a = cmds[i];
            let b = cmds[i + 1];

            if let Some(new_a) = maybe_simplify(a) {
                made_change = true;
                cmds[i] = new_a;
                // NB: this means we will process i again
                continue;
            }

            if let Some(combination) = maybe_combine(a, b) {
                made_change = true;
                cmds[i] = combination;
                cmds.remove(i + 1);
                // NB: this means we will process i again
                continue;
            }

            // otherwise proceed to swap
            if a > b && is_independent(a, b) {
                made_change = true;
                cmds[i] = b;
                cmds[i + 1] = a;
            }

            i += 1;
        }

        if !made_change {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test() {
        use Value::Integer;
        use Value::Variable as Var;

        let cmds = "inp w
add z w
mod z 2
div w 2
add y w
mul z 3
eql z x";

        let actual = parse(cmds);

        let expected = vec![
            Command::Inp(0),
            Command::TwoArg(3, TwoArgOperand::Add, Var(0)),
            Command::TwoArg(3, TwoArgOperand::Mod, Integer(2)),
            Command::TwoArg(0, TwoArgOperand::Div, Integer(2)),
            Command::TwoArg(2, TwoArgOperand::Add, Var(0)),
            Command::TwoArg(3, TwoArgOperand::Mul, Integer(3)),
            Command::TwoArg(3, TwoArgOperand::Eql, Var(1)),
        ];

        assert_eq!(actual, expected);
    }

    const BITS: &'static str = "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2";

    fn do_test(input: &str, vals: &[Int]) -> [Int; 4] {
        let parsed = parse(input);

        execute(&parsed, vals)
    }

    #[test]
    fn sample_thing() {
        let actual = do_test(BITS, &[13]);
        let expected = [1, 1, 0, 1];

        assert_eq!(actual, expected);

        let actual = do_test(BITS, &[12]);
        let expected = [1, 1, 0, 0];

        assert_eq!(actual, expected);

        let actual = do_test(BITS, &[128]);
        let expected = [0, 0, 0, 0];

        assert_eq!(actual, expected);
    }

    #[test]
    fn combine_test() {
        // these set commands set the same variable, so last write wins
        let a = Command::TwoArg(1, TwoArgOperand::Set, Value::Variable(0));
        let b = Command::TwoArg(1, TwoArgOperand::Set, Value::Integer(12));

        let act = maybe_combine(a, b);

        assert_eq!(act, Some(b));

        let act = maybe_combine(b, a);

        assert_eq!(act, Some(a));
    }

    const SIMPLE_OPT_EXAMPLE: &'static str = "inp w
mul x 0
add x z
mod x 26
div z 1
mul x 1
add x 10
eql x w
eql x 0
mul y 0";

    #[test]
    fn optimize_test_with_simplify() {
        use Value::Integer as Int;
        use Value::Variable as Var;

        let mut parsed = parse(SIMPLE_OPT_EXAMPLE);

        let expected = vec![
            Command::Inp(0),
            Command::TwoArg(1, TwoArgOperand::Mul, Int(0)),
            Command::TwoArg(1, TwoArgOperand::Add, Var(3)),
            Command::TwoArg(1, TwoArgOperand::Mod, Int(26)),
            Command::TwoArg(3, TwoArgOperand::Div, Int(1)),
            Command::TwoArg(1, TwoArgOperand::Mul, Int(1)),
            Command::TwoArg(1, TwoArgOperand::Add, Int(10)),
            Command::TwoArg(1, TwoArgOperand::Eql, Var(0)),
            Command::TwoArg(1, TwoArgOperand::Eql, Int(0)),
            Command::TwoArg(2, TwoArgOperand::Mul, Int(0)),
        ];

        assert_eq!(parsed, expected);

        optimize(&mut parsed);

        let expected = vec![
            // gotta bunch of explicit sets, and some reorders in there too
            Command::TwoArg(0, TwoArgOperand::Set, Int(0)),
            Command::TwoArg(1, TwoArgOperand::Set, Int(0)),
            Command::TwoArg(2, TwoArgOperand::Set, Int(0)),
            Command::TwoArg(3, TwoArgOperand::Set, Int(0)),
            Command::TwoArg(1, TwoArgOperand::Add, Var(3)),
            Command::TwoArg(1, TwoArgOperand::Mod, Int(26)),
            Command::TwoArg(1, TwoArgOperand::Add, Int(10)),
            // Command::TwoArg(3, TwoArgOperand::Div, Int(1)), // optimized away
            Command::Inp(0),
            // combined two equals into a neql
            Command::TwoArg(1, TwoArgOperand::Neq, Var(0)),
            Command::TwoArg(2, TwoArgOperand::Mul, Int(0)),
        ];

        assert_eq!(expected, parsed);
    }

    // a sample from input_24
    const OPT_EXAMPLE: &'static str = "inp w
mul x 0
add x z
mod x 26
div z 1
add x 10
eql x w
eql x 0
mul y 0";

    #[test]
    fn optimize_test() {
        use Value::Integer as Int;
        use Value::Variable as Var;

        let mut parsed = parse(OPT_EXAMPLE);

        let expected = vec![
            Command::Inp(0),
            Command::TwoArg(1, TwoArgOperand::Mul, Int(0)),
            Command::TwoArg(1, TwoArgOperand::Add, Var(3)),
            Command::TwoArg(1, TwoArgOperand::Mod, Int(26)),
            Command::TwoArg(3, TwoArgOperand::Div, Int(1)),
            Command::TwoArg(1, TwoArgOperand::Add, Int(10)),
            Command::TwoArg(1, TwoArgOperand::Eql, Var(0)),
            Command::TwoArg(1, TwoArgOperand::Eql, Int(0)),
            Command::TwoArg(2, TwoArgOperand::Mul, Int(0)),
        ];

        assert_eq!(parsed, expected);

        optimize(&mut parsed);

        let expected = vec![
            // gotta bunch of explicit sets, and some reorders in there too
            Command::TwoArg(0, TwoArgOperand::Set, Int(0)),
            Command::TwoArg(1, TwoArgOperand::Set, Int(0)),
            Command::TwoArg(2, TwoArgOperand::Set, Int(0)),
            Command::TwoArg(3, TwoArgOperand::Set, Int(0)),
            Command::TwoArg(1, TwoArgOperand::Add, Var(3)),
            Command::TwoArg(1, TwoArgOperand::Mod, Int(26)),
            Command::TwoArg(1, TwoArgOperand::Add, Int(10)),
            // Command::TwoArg(3, TwoArgOperand::Div, Int(1)), // optimized away
            Command::Inp(0),
            // combined two equals into a neql
            Command::TwoArg(1, TwoArgOperand::Neq, Var(0)),
            Command::TwoArg(2, TwoArgOperand::Mul, Int(0)),
        ];

        assert_eq!(expected, parsed);
    }
}
