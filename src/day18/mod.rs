use nom::{bytes::complete::tag, character::complete::digit1, combinator::map, sequence::tuple, IResult};

fn input() -> String {
    std::fs::read_to_string("input/input_18.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> u64 {
    let mut lines = input.lines();

    let mut running = parse(lines.next().unwrap());

    for line in lines {
        let b = parse(line);
        running = add(running, b);
        running = reduce(running);
    }

    magnitude(&running)
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> u64 {
    let nums: Vec<SnailNum> = input.lines().map(parse).collect();

    let mut max_sum = u64::MIN;

    for i in 0..nums.len() {
        for j in 0..nums.len() {
            if i == j {
                continue;
            }

            let sum = reduce(add(nums[i].clone(), nums[j].clone()));
            let mag = magnitude(&sum);

            max_sum = max_sum.max(mag);
        }
    }

    max_sum
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum Element {
    Num(u64),
    Pair(Box<SnailNum>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct SnailNum(Element, Element);

fn add(a: SnailNum, b: SnailNum) -> SnailNum {
    SnailNum(Element::Pair(Box::new(a)), Element::Pair(Box::new(b)))
}

fn reduce(sn: SnailNum) -> SnailNum {
    let mut running = sn;

    loop {
        // try explosion
        let (new_running, made_change) = try_explode(running);
        running = new_running;
        if made_change {
            continue;
        }

        // try splitting
        let (new_running, made_change) = try_split(running);
        running = new_running;
        if made_change {
            continue;
        }

        return running;
    }
}

fn try_explode(sn: SnailNum) -> (SnailNum, bool) {
    let mut sn = Element::Pair(Box::new(sn));

    // idea:
    //  1.  go through and find the first explode "index" if possible
    //  2.  replace that index (which should be a pair of numbers) with 0 and mark its left and right neighbors to

    let mut explode_found = false;
    // this is the index of a _number_, not an element in general
    let mut running_index = 0;
    let mut left_overflow = 0;
    let mut right_overflow = 0;

    fn find_explosion(
        elt: &mut Element,
        running_index: &mut usize,
        depth: usize,
        left_overflow: &mut u64,
        right_overflow: &mut u64,
        explode_found: &mut bool,
    ) {
        if *explode_found {
            return;
        }

        match elt {
            Element::Num(_) => {
                // don't explode this number; point the index at the next one
                *running_index += 1;
            }
            Element::Pair(sn) => {
                if depth == 4 {
                    match sn.as_ref() {
                        SnailNum(Element::Num(left_val), Element::Num(right_val)) => {
                            *explode_found = true;
                            *left_overflow = *left_val;
                            *right_overflow = *right_val;
                            *elt = Element::Num(0);
                        }
                        SnailNum(a, b) => {
                            panic!("Can't explode a value that looks like {:?} , {:?}", a, b);
                        }
                    }
                } else {
                    let snr = sn.as_mut();
                    let left = &mut snr.0;
                    find_explosion(left, running_index, depth + 1, left_overflow, right_overflow, explode_found);

                    let right = &mut snr.1;
                    find_explosion(right, running_index, depth + 1, left_overflow, right_overflow, explode_found);
                }
            }
        }
    }

    find_explosion(
        &mut sn,
        &mut running_index,
        0,
        &mut left_overflow,
        &mut right_overflow,
        &mut explode_found,
    );

    // running_index is now the index of an exploded point (which is now zero)

    fn traverse_and_add(sn: &mut Element, running_index: &mut usize, target_index: usize, left_overflow_amt: u64, right_overflow_amt: u64) {
        match *sn {
            Element::Num(ref mut val_ref) => {
                if *running_index + 1 == target_index {
                    *val_ref += left_overflow_amt;
                } else if target_index + 1 == *running_index {
                    *val_ref += right_overflow_amt;
                }
                *running_index += 1;
            }
            Element::Pair(ref mut sn) => {
                let SnailNum(a, b) = sn.as_mut();
                traverse_and_add(a, running_index, target_index, left_overflow_amt, right_overflow_amt);
                traverse_and_add(b, running_index, target_index, left_overflow_amt, right_overflow_amt);
            }
        }
    }

    if explode_found {
        traverse_and_add(&mut sn, &mut 0, running_index, left_overflow, right_overflow);
    }

    // todo: apply overflow

    match sn {
        Element::Pair(sn) => (*sn, explode_found),
        Element::Num(_) => panic!("Somehow exploded a pair into a number, that doesn't seem right"),
    }
}

fn try_split(sn: SnailNum) -> (SnailNum, bool) {
    fn recurse(sn: &mut Element) -> bool {
        match *sn {
            Element::Num(ref mut v) => {
                if *v < 10 {
                    false
                } else {
                    let left = *v / 2;
                    let right = (*v + 1) / 2;
                    assert_eq!(left + right, *v);
                    *sn = Element::Pair(Box::new(SnailNum(Element::Num(left), Element::Num(right))));
                    true
                }
            }
            Element::Pair(ref mut child) => {
                let &mut SnailNum(ref mut a, ref mut b) = child.as_mut();

                recurse(a) || recurse(b)
            }
        }
    }

    let mut sn = Element::Pair(Box::new(sn));
    let did_split = recurse(&mut sn);

    match sn {
        Element::Pair(sn) => (*sn, did_split),
        Element::Num(_) => panic!("Somehow exploded a pair into a number, that doesn't seem right"),
    }
}

fn magnitude(sn: &SnailNum) -> u64 {
    fn helper(e: &Element) -> u64 {
        match e {
            Element::Num(v) => *v,
            Element::Pair(b) => {
                let sn = b.as_ref();
                3 * helper(&sn.0) + 2 * helper(&sn.1)
            }
        }
    }

    3 * helper(&sn.0) + 2 * helper(&sn.1)
}

fn parse(input: &str) -> SnailNum {
    let input: String = input.chars().filter(|c| !c.is_whitespace()).collect();

    fn parse_pair(input: &str) -> IResult<&str, SnailNum> {
        let (input, (_, a, _, b, _)) = tuple((tag("["), parse_element, tag(","), parse_element, tag("]")))(input)?;

        let sn = SnailNum(a, b);

        Ok((input, sn))
    }

    fn parse_num(input: &str) -> IResult<&str, Element> {
        let (input, val) = map(digit1, |digits: &str| digits.parse::<u64>().unwrap())(input)?;

        Ok((input, Element::Num(val)))
    }

    fn parse_element(input: &str) -> IResult<&str, Element> {
        if input.starts_with('[') {
            let (input, sn) = parse_pair(input)?;
            Ok((input, Element::Pair(Box::new(sn))))
        } else {
            parse_num(input)
        }
    }

    let (leftover, num) = parse_pair(&input).unwrap();
    assert!(leftover.is_empty());
    num
}

#[cfg(test)]
mod tests {
    use super::*;

    fn b<T>(t: T) -> Box<T> {
        Box::new(t)
    }

    #[test]
    fn parse_test1() {
        let actual = parse("[1,1]");
        let expected = SnailNum(Element::Num(1), Element::Num(1));

        assert_eq!(actual, expected)
    }

    #[test]
    fn parse_test2() {
        let actual = parse("[1,[[4, 5], 3]]");
        let expected = SnailNum(
            Element::Num(1),
            Element::Pair(b(SnailNum(
                Element::Pair(b(SnailNum(Element::Num(4), Element::Num(5)))),
                Element::Num(3),
            ))),
        );

        assert_eq!(actual, expected)
    }

    #[test]
    fn magnitude_test() {
        fn do_test(input: &str, expected: u64) {
            let parsed = parse(input);
            let actual = magnitude(&parsed);

            assert_eq!(actual, expected);
        }

        do_test("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445);
    }

    #[test]
    fn explode_test() {
        fn test_does_explode(input: &str, expected: &str) {
            let input = parse(input);
            let expected = parse(expected);

            let (actual, did_change) = try_explode(input);

            assert!(did_change);
            assert_eq!(actual, expected);
        }

        for (input, expected) in [
            ("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
            ("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
            // captures the case where you need to push something off the right and way off
            // into another land
            ("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
            // reversed copy of the above, to capture the left thing
            ("[1,[[[[2,3],4],5],6]]", "[3,[[[0,7],5],6]]"),
            ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
            ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"),
        ] {
            test_does_explode(input, expected);
        }
    }

    #[test]
    fn reduce_test_1() {
        assert_eq!(
            reduce(parse("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")),
            parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
        )
    }

    #[test]
    fn sample_a1() {
        let input = "[1,1]
[2,2]
[3,3]
[4,4]";
        let actual = a_with_input(input);
        let expected = magnitude(&parse("[[[[1,1],[2,2]],[3,3]],[4,4]]"));

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a2() {
        let input = "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]";
        let actual = a_with_input(input);
        let expected = magnitude(&parse("[[[[3,0],[5,3]],[4,4]],[5,5]]"));

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a3() {
        let input = "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]";
        let actual = a_with_input(input);
        let expected = magnitude(&parse("[[[[5,0],[7,4]],[5,5]],[6,6]]"));

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a4() {
        let input = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]";
        let actual = a_with_input(input);
        let expected = magnitude(&parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"));

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a5() {
        let input = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]";
        let actual = a_with_input(input);
        let expected = magnitude(&parse("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"));

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(
            "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]",
        );
        let expected = 3993;

        assert_eq!(expected, actual);
    }
}
