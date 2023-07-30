use std::fmt;
use std::ops::Not;
use std::rc::Rc;

use super::bounds::{add_bounds, are_disjoint, div_bounds, is_contained_in, mul_bounds, union, Bounds};
use super::cmd::{Command, Int, OneArgOperand, TwoArgOperand, Value};

pub fn collect_conditions(b: &BoundedSV) -> Vec<BoundedSV> {
    fn recurse(b: &BoundedSV, found: &mut Vec<BoundedSV>) {
        match &b.sv {
            SymbolicValue::Input(_) => {}
            SymbolicValue::Integer(_) => {}
            SymbolicValue::Derived(a, _, b) => {
                recurse(a.as_ref(), found);
                recurse(b.as_ref(), found);
            }
            SymbolicValue::Ternary(cond, a, b) => {
                if found.iter().any(|known| known == cond.as_ref()).not() {
                    found.push(cond.as_ref().clone());
                }
                recurse(a.as_ref(), found);
                recurse(b.as_ref(), found);
            }
            SymbolicValue::DerivedOne(OneArgOperand::Mod26, arg) => {
                recurse(arg.as_ref(), found);
            }
        }
    }

    let mut running = Vec::new();

    recurse(b, &mut running);

    running
}

// I0 is first read, I1 is next, etc. So the index is just the "read number"
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct InputValue(usize);

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum SymbolicValue {
    DerivedOne(OneArgOperand, Rc<BoundedSV>),
    // might want to end up doing Rc or something here but not right this second
    // but there are a LOT of clones, idk if it will end up mattering
    Derived(Rc<BoundedSV>, TwoArgOperand, Rc<BoundedSV>),
    // cond, val_if_true, val_if_false
    // i'm not in love with the use of BoundedSV for the condition, i think
    Ternary(Rc<BoundedSV>, Rc<BoundedSV>, Rc<BoundedSV>),
    Input(InputValue),
    Integer(Int),
}

impl fmt::Debug for BoundedSV {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.sv)
    }
}

impl BoundedSV {
    pub fn to_string(&self, start_indent: usize) -> String {
        let mut out = String::new();
        self.pretty_print(start_indent, &mut out);
        out
    }

    fn pretty_print(&self, indent_spaces: usize, out: &mut String) {
        self.sv.pretty_print(indent_spaces, out)
    }
}

impl SymbolicValue {
    fn pretty_print(&self, indent_spaces: usize, out: &mut String) {
        fn newline(out: &mut String, indent: usize) {
            out.push('\n');
            for _ in 0..indent {
                out.push(' ');
            }
        }

        match self {
            SymbolicValue::Integer(i) => out.push_str(i.to_string().as_str()),
            SymbolicValue::Input(InputValue(i)) => {
                out.push('W');
                out.push_str(i.to_string().as_str())
            }
            SymbolicValue::DerivedOne(OneArgOperand::Mod26, a) => {
                out.push('(');
                a.sv.pretty_print(indent_spaces, out);
                out.push_str(" % 26)")
            }
            SymbolicValue::Derived(a, op, b) => {
                out.push('(');
                a.pretty_print(indent_spaces, out);
                out.push(' ');
                out.push_str(op.to_string().as_str());
                out.push(' ');
                b.pretty_print(indent_spaces, out);
                out.push(')');
            }
            SymbolicValue::Ternary(cond, if_true, if_false) => {
                /* Looks like this:
                   IF
                       cond
                   THEN
                       stuff
                   ELSE
                       more_stuff
                */
                out.push_str("IF ");
                newline(out, indent_spaces + 4);
                cond.pretty_print(indent_spaces + 4, out);

                newline(out, indent_spaces);
                out.push_str("THEN");
                newline(out, indent_spaces + 4);
                if_true.pretty_print(indent_spaces + 4, out);

                newline(out, indent_spaces);
                out.push_str("ELSE");
                newline(out, indent_spaces + 4);
                if_false.pretty_print(indent_spaces + 4, out);
            }
        }
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
                write!(f, "(IF ({:?}) THEN ({:?}) ELSE ({:?}))", cond, if_true, if_false)
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
                    }
                )?;
                write!(f, "{:?})", b)?;
                Ok(())
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct BoundedSV {
    pub sv: SymbolicValue,
    pub bounds: Bounds,
}

fn const_sv(val: Int) -> BoundedSV {
    BoundedSV {
        sv: SymbolicValue::Integer(val),
        bounds: (Some(val), Some(val)),
    }
}

pub fn symbolic_execute(cmds: &[Command]) -> [BoundedSV; 4] {
    let mut state = [const_sv(0), const_sv(0), const_sv(0), const_sv(0)];
    let mut inp_counter = 0;

    for cmd in cmds {
        iter_state(&mut inp_counter, &mut state, cmd.clone());
    }

    state
}

fn iter_state(inp_counter: &mut usize, state: &mut [BoundedSV; 4], cmd: Command) {
    fn eval(state: &[BoundedSV; 4], val: Value) -> BoundedSV {
        match val {
            Value::Integer(i) => const_sv(i),
            Value::Variable(var) => state[var as usize].clone(),
        }
    }

    match cmd {
        Command::TwoArg(var, op, val) => {
            let lhs = state[var as usize].clone();
            let rhs = eval(state, val);
            let new_val: BoundedSV = match op {
                TwoArgOperand::Set => rhs,
                TwoArgOperand::Add => add_two(lhs, rhs),
                TwoArgOperand::Mul => mul_two(lhs, rhs),
                TwoArgOperand::Div => div_two(lhs, rhs),
                TwoArgOperand::Mod => mod_two(lhs, rhs),
                TwoArgOperand::Eql => eql_two(lhs, rhs),
            };
            let new_val = prune_tree(new_val);
            state[var as usize] = new_val;
        }
        Command::OneArg(var, op) => {
            let lhs = state[var as usize].clone();
            let new_val: BoundedSV = match op {
                OneArgOperand::Mod26 => mod_26(lhs),
            };
            let new_val = prune_tree(new_val);
            state[var as usize] = new_val;
        }
        Command::Inp(var) => {
            let new_val = BoundedSV {
                bounds: (Some(1), Some(9)),
                sv: SymbolicValue::Input(InputValue(*inp_counter)),
            };
            *inp_counter += 1;
            let new_val = prune_tree(new_val);
            state[var as usize] = new_val;
        }
    };
}

pub fn prune_conditions(val: BoundedSV, known_true: &mut Vec<BoundedSV>, known_false: &mut Vec<BoundedSV>) -> BoundedSV {
    let old_bounds = val.bounds;
    match val.sv {
        SymbolicValue::DerivedOne(OneArgOperand::Mod26, arg) => {
            let arg = prune_conditions(arg.as_ref().clone(), known_true, known_false);
            // TODO: we can probably tighten up these bounds
            let bounds = old_bounds;
            let sv = SymbolicValue::DerivedOne(OneArgOperand::Mod26, arg.into());

            BoundedSV { sv, bounds }
        }
        SymbolicValue::Derived(lhs, op, rhs) => {
            let lhs = prune_conditions(lhs.as_ref().clone(), known_true, known_false);
            let rhs = prune_conditions(rhs.as_ref().clone(), known_true, known_false);

            // TODO: we can probably tighten up these bounds
            let bounds = old_bounds;
            let sv = SymbolicValue::Derived(lhs.into(), op, rhs.into());

            BoundedSV { sv, bounds }
        }
        SymbolicValue::Ternary(cond, if_true, if_false) => {
            if known_true.contains(cond.as_ref()) {
                prune_conditions(if_true.as_ref().clone(), known_true, known_false)
            } else if known_false.contains(cond.as_ref()) {
                prune_conditions(if_false.as_ref().clone(), known_true, known_false)
            } else {
                let cond = cond.as_ref().clone();
                known_true.push(cond);
                let if_true = prune_conditions(if_true.as_ref().clone(), known_true, known_false);
                let cond = known_true.pop().unwrap();
                known_false.push(cond);
                let if_false = prune_conditions(if_false.as_ref().clone(), known_true, known_false);
                let cond = known_false.pop().unwrap();
                let bounds = union(if_true.bounds, if_false.bounds);

                // sometimes after collapsing we notice the arms are identical; then we can skip
                // the entire conditional
                if if_true == if_false {
                    if_true
                } else {
                    let sv = SymbolicValue::Ternary(cond.into(), if_true.into(), if_false.into());
                    BoundedSV { bounds, sv }
                }
            }
        }
        SymbolicValue::Input(i) => BoundedSV {
            bounds: old_bounds,
            sv: SymbolicValue::Input(i),
        },
        SymbolicValue::Integer(i) => const_sv(i),
    }
}

fn prune_tree(val: BoundedSV) -> BoundedSV {
    let mut true_stack = Vec::new();
    let mut false_stack = Vec::new();

    prune_conditions(val, &mut true_stack, &mut false_stack)
}

#[inline(always)]
fn is_zero(bv: &BoundedSV) -> bool {
    is_val(bv, 0)
}

#[inline(always)]
fn is_one(bv: &BoundedSV) -> bool {
    is_val(bv, 1)
}

fn is_val(bv: &BoundedSV, val: Int) -> bool {
    match bv {
        BoundedSV {
            sv: SymbolicValue::Integer(a),
            ..
        } => *a == val,
        _ => false,
    }
}

fn add_two(lhs: BoundedSV, rhs: BoundedSV) -> BoundedSV {
    match (lhs, rhs) {
        (lhs, rhs) if is_zero(&lhs) => rhs,
        (lhs, rhs) if is_zero(&rhs) => lhs,
        (
            BoundedSV {
                sv: SymbolicValue::Integer(a),
                ..
            },
            BoundedSV {
                sv: SymbolicValue::Integer(b),
                ..
            },
        ) => {
            let new_val = a.checked_add(b).expect("Oh no addition overflow");
            const_sv(new_val)
        }
        (lhs, rhs) => {
            if is_ternary(&rhs) {
                let SymbolicValue::Ternary(cond, if_true, if_false) = rhs.sv else {
                    unreachable!()
                };
                let if_true = add_two(lhs.clone(), if_true.as_ref().clone());
                let if_false = add_two(lhs, if_false.as_ref().clone());
                let bounds = union(if_true.bounds, if_false.bounds);
                let sv = SymbolicValue::Ternary(cond, if_true.into(), if_false.into());
                BoundedSV { bounds, sv }
            } else if is_ternary(&lhs) {
                let SymbolicValue::Ternary(cond, if_true, if_false) = lhs.sv else {
                    unreachable!()
                };
                let if_true = add_two(if_true.as_ref().clone(), rhs.clone());
                let if_false = add_two(if_false.as_ref().clone(), rhs);
                let bounds = union(if_true.bounds, if_false.bounds);
                let sv = SymbolicValue::Ternary(cond, if_true.into(), if_false.into());
                BoundedSV { bounds, sv }
            } else {
                let bounds = add_bounds(lhs.bounds, rhs.bounds);
                let sv = SymbolicValue::Derived(Rc::new(lhs), TwoArgOperand::Add, Rc::new(rhs));
                BoundedSV { bounds, sv }
            }
        }
    }
}

fn mul_two(lhs: BoundedSV, rhs: BoundedSV) -> BoundedSV {
    match (lhs, rhs) {
        (lhs, rhs) if is_one(&lhs) => rhs,
        (lhs, rhs) if is_one(&rhs) => lhs,
        (lhs, rhs) if is_zero(&lhs) || is_zero(&rhs) => const_sv(0),
        (
            BoundedSV {
                sv: SymbolicValue::Integer(lhs),
                ..
            },
            BoundedSV {
                sv: SymbolicValue::Integer(rhs),
                ..
            },
        ) => {
            let new_val = lhs.checked_mul(rhs).expect("Oh no mult overflow");
            const_sv(new_val)
        }
        (lhs, rhs) => {
            if is_ternary(&rhs) {
                let SymbolicValue::Ternary(cond, if_true, if_false) = rhs.sv else {
                    unreachable!()
                };
                let if_true = mul_two(lhs.clone(), if_true.as_ref().clone());
                let if_false = mul_two(lhs, if_false.as_ref().clone());
                let bounds = union(if_true.bounds, if_false.bounds);
                let sv = SymbolicValue::Ternary(cond, if_true.into(), if_false.into());
                BoundedSV { bounds, sv }
            } else if is_ternary(&lhs) {
                let SymbolicValue::Ternary(cond, if_true, if_false) = lhs.sv else {
                    unreachable!()
                };
                let if_true = mul_two(if_true.as_ref().clone(), rhs.clone());
                let if_false = mul_two(if_false.as_ref().clone(), rhs);
                let bounds = union(if_true.bounds, if_false.bounds);
                let sv = SymbolicValue::Ternary(cond, if_true.into(), if_false.into());
                BoundedSV { bounds, sv }
            } else {
                let bounds = mul_bounds(lhs.bounds, rhs.bounds);
                let sv = SymbolicValue::Derived(lhs.into(), TwoArgOperand::Mul, rhs.into());
                BoundedSV { bounds, sv }
            }
        }
    }
}

fn div_two(lhs: BoundedSV, rhs: BoundedSV) -> BoundedSV {
    /// If term is of the form (A*26 + B) and B is bounded within [0, 25]
    /// then return A (otherwise return nothing)
    fn try_divide_26(term: &BoundedSV, divisor: &BoundedSV) -> Option<BoundedSV> {
        if !is_val(divisor, 26) {
            return None;
        }

        match &term.sv {
            SymbolicValue::Derived(lhs, TwoArgOperand::Add, rhs) => {
                // if rhs is too big then the whole thing is off, too complicated with
                // remainders and stuff
                if !is_contained_in(rhs.bounds, (Some(0), Some(25))) {
                    None
                } else {
                    let out: Option<BoundedSV> = match lhs.sv.clone() {
                        SymbolicValue::Derived(a, TwoArgOperand::Mul, b) if is_val(b.as_ref(), 26) => {
                            let a: BoundedSV = a.as_ref().clone();
                            Some(a)
                        }
                        _ => None,
                    };
                    out
                }
            }
            _ => None,
        }
    }

    match (lhs, rhs) {
        (lhs, _) if is_zero(&lhs) => const_sv(0),
        (lhs, rhs) if is_one(&rhs) => lhs,
        (
            BoundedSV {
                sv: SymbolicValue::Integer(a),
                ..
            },
            BoundedSV {
                sv: SymbolicValue::Integer(b),
                ..
            },
        ) => {
            if a < 0 || b <= 0 {
                panic!("IDR how negative numbers work in division; got {} / {}", a, b);
            }
            let new_val = a / b;
            const_sv(new_val)
        }
        (lhs, rhs) => {
            if is_ternary(&rhs) {
                let SymbolicValue::Ternary(cond, if_true, if_false) = rhs.sv else {
                    unreachable!()
                };
                let if_true = div_two(lhs.clone(), if_true.as_ref().clone());
                let if_false = div_two(lhs, if_false.as_ref().clone());
                let bounds = union(if_true.bounds, if_false.bounds);
                let sv = SymbolicValue::Ternary(cond, if_true.into(), if_false.into());
                BoundedSV { bounds, sv }
            } else if is_ternary(&lhs) {
                let SymbolicValue::Ternary(cond, if_true, if_false) = lhs.sv else {
                    unreachable!()
                };
                let if_true = div_two(if_true.as_ref().clone(), rhs.clone());
                let if_false = div_two(if_false.as_ref().clone(), rhs);
                let bounds = union(if_true.bounds, if_false.bounds);
                let sv = SymbolicValue::Ternary(cond, if_true.into(), if_false.into());
                BoundedSV { bounds, sv }
            } else if let Some(quotient) = try_divide_26(&lhs, &rhs) {
                // now this is AWFULLY specific but it comes up a lot
                // basically we want ((A * 26) + (small thing)) / 26
                // to simplify to A
                quotient
            } else {
                let bounds = div_bounds(lhs.bounds, rhs.bounds);
                let sv = SymbolicValue::Derived(Rc::new(lhs), TwoArgOperand::Div, Rc::new(rhs));
                BoundedSV { bounds, sv }
            }
        }
    }
}

fn mod_two(_lhs: BoundedSV, _rhs: BoundedSV) -> BoundedSV {
    unreachable!("I think we optimized all the non-26 mods away")
}

fn mod_26(lhs: BoundedSV) -> BoundedSV {
    match lhs {
        BoundedSV {
            sv: SymbolicValue::Integer(i),
            ..
        } => {
            if i < 0 {
                panic!("idr how to mod a negative number; got {:?}", i);
            }
            let new_val = i % 26;
            const_sv(new_val)
        }
        lhs => {
            let mod_bounds = (Some(0), Some(25));
            if is_contained_in(lhs.bounds, mod_bounds) {
                lhs
            } else if is_ternary(&lhs) {
                let SymbolicValue::Ternary(cond, if_true, if_false) = lhs.sv else {
                    unreachable!()
                };
                let if_true = mod_26(if_true.as_ref().clone());
                let if_false = mod_26(if_false.as_ref().clone());
                let bounds = union(if_true.bounds, if_false.bounds);
                let sv = SymbolicValue::Ternary(cond, if_true.into(), if_false.into());
                BoundedSV { bounds, sv }
            } else {
                let original_bounds = lhs.bounds;
                match lhs.sv {
                    // this is the main one that matters; (a * 26) % 26 is zero
                    SymbolicValue::Derived(_, TwoArgOperand::Mul, rhs) if is_val(&rhs, 26) => const_sv(0),
                    // but in order to detect this stuff, we need to distribute across addition
                    SymbolicValue::Derived(a, TwoArgOperand::Add, b) => {
                        let a_mod_26 = mod_26(a.as_ref().clone());
                        let b_mod_26 = mod_26(b.as_ref().clone());
                        if is_zero(&a_mod_26) {
                            b_mod_26
                        } else if is_zero(&b_mod_26) {
                            a_mod_26
                        } else {
                            unimplemented!("Need to distribute! ({:?} + {:?}) % 26", a_mod_26, b_mod_26)
                        }
                    }
                    // otherwise there's really nothing to be done, just stop
                    arg => {
                        let out = BoundedSV {
                            // the bounds get all screwy; in some cases we
                            // might be able to improve this, but idk right now
                            bounds: mod_bounds,
                            sv: SymbolicValue::DerivedOne(
                                OneArgOperand::Mod26,
                                BoundedSV {
                                    sv: arg,
                                    bounds: original_bounds,
                                }
                                .into(),
                            ),
                        };
                        println!("Couldn't simplify a mod; returning {:?}", out);
                        out
                    }
                }
            }
        }
    }
}

fn is_ternary(a: &BoundedSV) -> bool {
    match &a.sv {
        &SymbolicValue::Ternary(_, _, _) => true,
        _ => false,
    }
}

fn eql_two(lhs: BoundedSV, rhs: BoundedSV) -> BoundedSV {
    if are_disjoint(lhs.bounds, rhs.bounds) {
        const_sv(0)
    } else if lhs == rhs {
        const_sv(1)
    } else if is_ternary(&rhs) {
        let SymbolicValue::Ternary(cond, if_true, if_false) = rhs.sv else {
                unreachable!()
            };
        let if_true = eql_two(lhs.clone(), if_true.as_ref().clone());
        let if_false = eql_two(lhs, if_false.as_ref().clone());
        let bounds = union(if_true.bounds, if_false.bounds);
        let sv = SymbolicValue::Ternary(cond, if_true.into(), if_false.into());
        BoundedSV { bounds, sv }
    } else if is_ternary(&lhs) {
        let SymbolicValue::Ternary(cond, if_true, if_false) = lhs.sv else {
                unreachable!()
            };
        let if_true = eql_two(if_true.as_ref().clone(), rhs.clone());
        let if_false = eql_two(if_false.as_ref().clone(), rhs);
        let bounds = union(if_true.bounds, if_false.bounds);
        let sv = SymbolicValue::Ternary(cond, if_true.into(), if_false.into());
        BoundedSV { bounds, sv }
    } else {
        BoundedSV {
            bounds: (Some(0), Some(1)),
            sv: SymbolicValue::Ternary(
                BoundedSV {
                    bounds: (Some(0), Some(1)),
                    sv: SymbolicValue::Derived(lhs.into(), TwoArgOperand::Eql, rhs.into()),
                }
                .into(),
                const_sv(1).into(),
                const_sv(0).into(),
            ),
        }
    }
}
