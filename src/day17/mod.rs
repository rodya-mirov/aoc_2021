fn input() -> String {
    std::fs::read_to_string("input/input_17.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> i32 {
    let ParseResult { xmin, xmax, ymin, ymax } = parse(input);

    assert!(xmin <= xmax);
    assert!(ymin <= ymax);

    // this is true of all samples and dramatically alters the math
    // so let's just assert it
    assert!(ymax < 0);

    // likewise here are some more things that are true and affect the math
    assert!(xmin > 0);

    // obviously can't go faster than this, although in practice this is super sparse
    // we can go back and be more choosy if performance turns out to matter
    let xvelmax = xmax;

    // this can actually be improved, if xmin is positive, but again, no point doing a bunch
    // of math if this is gonna run in half a ms
    let xvelmin = 0;

    // since ymax < 0, if yvel > 0, then what you get is it goes up for yvel timesteps, then
    // comes back down for yvel timesteps, arriving precisely at y=0 with new velocity -yvel
    // and maximum height achieved of (yvel * (yvel + 1)) / 2
    //
    // otoh if yvel < 0 at the beginning then the highest achieved y is zero because it just
    // goes down (i don't expect this to be competitive except for degenerate input?)
    //
    // anyway with that in mind you can get some bounds on yvel -- if abs(yvel) > abs(ymin)
    // then it'll overshoot the entire box
    let yvelmin = ymin;
    let yvelmax = -ymin;

    for xv in xvelmin..xvelmax {
        // from the x perspective, there are sort of two interesting cases -- when you'll
        // eventually pass through the box, and when you'll linger there forever
        let final_pos = (xv * (xv + 1)) / 2;
        if final_pos < xmin {
            // we might never get there in which case this xvel is simply too low to be interesting
            continue;
        }

        // we can figure out the times that are available for this xv value
        let (tmin, tmax) = {
            let mut tmin = i32::MAX;
            let mut tmax = i32::MIN;

            let mut xv = xv;
            let mut x = 0;
            let mut t = 0;

            while xv > 0 {
                x += xv;
                xv -= 1;
                t += 1;

                if x > xmax {
                    break;
                } else if x >= xmin {
                    tmin = tmin.min(t);
                    tmax = tmax.max(t);
                }
            }

            if xv == 0 && x >= xmin && x <= xmax {
                tmax = i32::MAX;
            }

            (tmin, tmax)
        };

        // then we can try some y-values; if we start at the top we can bail out on first solution
        for yv in (yvelmin..=yvelmax).rev() {
            if yv > 0 {
                let max_height = (yv * (yv + 1)) / 2;

                let mut t = 2 * yv;
                let mut y = 0;
                let mut yv = -yv - 1;

                loop {
                    if y < ymin || t > tmax {
                        break;
                    } else if t >= tmin && y <= ymax {
                        return max_height;
                    }

                    y += yv;
                    yv -= 1;
                    t += 1;
                }
            }
        }
    }

    unimplemented!("No solution found")
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> u64 {
    let ParseResult { xmin, xmax, ymin, ymax } = parse(input);

    assert!(xmin <= xmax);
    assert!(ymin <= ymax);

    // this is true of all samples and dramatically alters the math
    // so let's just assert it
    assert!(ymax < 0);

    // likewise here are some more things that are true and affect the math
    assert!(xmin > 0);

    // obviously can't go faster than this, although in practice this is super sparse
    // we can go back and be more choosy if performance turns out to matter
    let xvelmax = xmax;

    // this can actually be improved, if xmin is positive, but again, no point doing a bunch
    // of math if this is gonna run in half a ms
    let xvelmin = 0;

    // since ymax < 0, if yvel > 0, then what you get is it goes up for yvel timesteps, then
    // comes back down for yvel timesteps, arriving precisely at y=0 with new velocity -yvel
    // and maximum height achieved of (yvel * (yvel + 1)) / 2
    //
    // otoh if yvel < 0 at the beginning then the highest achieved y is zero because it just
    // goes down (i don't expect this to be competitive except for degenerate input?)
    //
    // anyway with that in mind you can get some bounds on yvel -- if abs(yvel) > abs(ymin)
    // then it'll overshoot the entire box
    let yvelmin = ymin;
    let yvelmax = -ymin;

    let mut solutions = 0;

    for xv in xvelmin..=xvelmax {
        // from the x perspective, there are sort of two interesting cases -- when you'll
        // eventually pass through the box, and when you'll linger there forever
        let final_pos = (xv * (xv + 1)) / 2;
        if final_pos < xmin {
            // we might never get there in which case this xvel is simply too low to be interesting
            continue;
        }

        // we can figure out the times that are available for this xv value
        let (tmin, tmax) = {
            let mut tmin = i32::MAX;
            let mut tmax = i32::MIN;

            let mut xv = xv;
            let mut x = 0;
            let mut t = 0;

            while xv > 0 {
                x += xv;
                xv -= 1;
                t += 1;

                if x > xmax {
                    break;
                } else if x >= xmin {
                    tmin = tmin.min(t);
                    tmax = tmax.max(t);
                }
            }

            if xv == 0 && x >= xmin && x <= xmax {
                tmax = i32::MAX;
            }

            (tmin, tmax)
        };

        for yv in yvelmin..=yvelmax {
            let (mut t, mut y, mut yv) = (0, 0, yv);

            loop {
                if y < ymin || t > tmax {
                    break;
                } else if t >= tmin && y <= ymax {
                    solutions += 1;
                    break;
                }

                y += yv;
                yv -= 1;
                t += 1;
            }
        }
    }

    solutions
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
struct ParseResult {
    xmin: i32,
    xmax: i32,
    ymin: i32,
    ymax: i32,
}

fn parse(input: &str) -> ParseResult {
    let tokens: Vec<i32> = input
        // so janky but whatever
        .replace("target area: x=", "")
        .replace(" y=", "")
        .split(',')
        .flat_map(|t| t.split(".."))
        .map(|t| t.parse::<i32>().unwrap())
        .collect();

    ParseResult {
        xmin: tokens[0],
        xmax: tokens[1],
        ymin: tokens[2],
        ymax: tokens[3],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test() {
        let actual = parse("target area: x=20..30, y=-10..-5");
        let expected = ParseResult {
            xmin: 20,
            xmax: 30,
            ymin: -10,
            ymax: -5,
        };

        assert_eq!(actual, expected)
    }

    #[test]
    fn sample_a() {
        let actual = a_with_input("target area: x=20..30, y=-10..-5");
        let expected = 45;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input("target area: x=20..30, y=-10..-5");
        let expected = 112;
        // our FPs: 9, 1;

        assert_eq!(expected, actual);
    }
}
