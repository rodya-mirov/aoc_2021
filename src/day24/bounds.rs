pub use add::add_bounds;
pub use div::div_bounds;
pub use mul::mul_bounds;

use super::cmd::Int;

pub type Bounds = (Option<Int>, Option<Int>);

pub fn is_contained_in(a: Bounds, b: Bounds) -> bool {
    let lb_fits = match (a.0, b.0) {
        (_, None) => true,
        (None, _) => false,
        (a, b) => b <= a,
    };

    let ub_fits = match (a.1, b.1) {
        (_, None) => true,
        (None, _) => false,
        (a, b) => a <= b,
    };

    lb_fits && ub_fits
}

pub fn are_disjoint(a: Bounds, b: Bounds) -> bool {
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

mod add {
    use super::{Bounds, Int};

    fn add_or_none(a: Option<Int>, b: Option<Int>) -> Option<Int> {
        match (a, b) {
            (None, _) => None,
            (_, None) => None,
            // handles overflow by defaulting to None (which is signed infinity
            // in this case)
            (Some(a), Some(b)) => a.checked_add(b),
        }
    }

    pub fn add_bounds(lhs: Bounds, rhs: Bounds) -> Bounds {
        let ((llb, lub), (rlb, rub)) = (lhs, rhs);

        let lb = add_or_none(llb, rlb);
        let ub = add_or_none(lub, rub);

        (lb, ub)
    }
}

mod div {
    use super::Bounds;

    fn is_nonnegative(b: Bounds) -> bool {
        b.0.is_some() && b.0.unwrap() >= 0
    }

    fn is_positive(b: Bounds) -> bool {
        b.0.is_some() && b.0.unwrap() > 0
    }

    pub fn div_bounds(a: Bounds, b: Bounds) -> Bounds {
        if is_nonnegative(a) && is_positive(b) {
            let alb = a.0.unwrap();
            let blb = b.0.unwrap();

            let lb = match b.1 {
                None => Some(0),
                // smallest value possible is "small / big"
                Some(bub) => Some(alb / bub),
            };

            let ub = match a.1 {
                // b includes a positive number so "huge / fixed_n" is "huge"
                None => None,
                //
                Some(aub) => Some(aub / blb),
            };

            (lb, ub)
        } else {
            unimplemented!("Idk about div bounds: {:?} and {:?}", a, b)
        }
    }
}

pub fn contains(bounds: Bounds, val: Int) -> bool {
    match bounds {
        (None, None) => true,
        (None, Some(ub)) => val <= ub,
        (Some(lb), None) => lb <= val,
        (Some(lb), Some(ub)) => lb <= val && val <= ub,
    }
}

/// Returns the smallest Bounds object which contains the two given Bounds objects
pub fn union(a: Bounds, b: Bounds) -> Bounds {
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

mod mul {
    use super::{union, Bounds, Int};

    fn const_bounds(i: Int) -> Bounds {
        (Some(i), Some(i))
    }

    fn is_zero(bounds: Bounds) -> bool {
        bounds == (Some(0), Some(0))
    }

    fn is_everything(b: Bounds) -> bool {
        b.0.is_none() && b.1.is_none()
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

    fn safe_union(a: Option<Bounds>, b: Option<Bounds>) -> Option<Bounds> {
        match (a, b) {
            (None, None) => None,
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (Some(a), Some(b)) => Some(union(a, b)),
        }
    }

    // TODO: unit tests

    pub fn mul_bounds(a: Bounds, b: Bounds) -> Bounds {
        if is_zero(a) || is_zero(b) {
            const_bounds(0)
        } else if is_everything(a) || is_everything(b) {
            (None, None)
        } else {
            let a_neg_range = match to_neg(a) {
                None => None,
                Some(neg_a) => Some(negate(mul_bounds(negate(neg_a), b))),
            };

            let mut running_range = a_neg_range;

            let a_nonneg = to_nonneg(a);
            if a_nonneg.is_none() {
                return a_neg_range.expect("They can't both be empty, I think");
            }

            let a_nonneg = a_nonneg.unwrap();

            let b_neg_range = match to_neg(b) {
                None => None,
                Some(neg_b) => Some(negate(mul_bounds(a_nonneg, negate(neg_b)))),
            };

            running_range = safe_union(running_range, b_neg_range);

            let b_nonneg = to_nonneg(b);
            if b_nonneg.is_none() {
                return running_range.expect("I feel like we must have found something");
            }

            // now we can just consider the nonnegative parts of both, which is much easier
            let (Some(a_lb), a_ub) = a else { unreachable!("We ensured bounds >= 0")};
            let (Some(b_lb), b_ub) = b else { unreachable!("We ensured b bounds >= 0")};

            let lb = a_lb.checked_mul(b_lb).expect("Bounded mul overflow oh noooo");

            let ub = a_ub.and_then(|a_ub_val| b_ub.and_then(|b_ub_val| a_ub_val.checked_mul(b_ub_val)));

            let nonneg_part: Option<Bounds> = Some((Some(lb), ub));

            safe_union(nonneg_part, running_range).expect("At least one part is nontrivial")
        }
    }
}
