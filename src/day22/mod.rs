use nom::Slice;

fn input() -> String {
    std::fs::read_to_string("input/input_22.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> usize {
    let ParseResult { lines } = parse(input);

    const MIN_COORD: i32 = -50;
    const MAX_COORD: i32 = 50;
    const LEN: usize = (MAX_COORD - MIN_COORD + 1) as usize;

    // index: grid[x][y][z]; no real significance just what we're doing
    let mut grid = [[[false; LEN]; LEN]; LEN];

    for Line {
        on,
        xmin,
        xmax,
        ymin,
        ymax,
        zmin,
        zmax,
    } in lines.iter().copied()
    {
        if xmin > MAX_COORD
            || ymin > MAX_COORD
            || zmin > MAX_COORD
            || xmax < MIN_COORD
            || ymax < MIN_COORD
            || zmax < MIN_COORD
        {
            continue;
        }

        let clamp = |p: i32| p.max(MIN_COORD).min(MAX_COORD);

        // perf: could do this clamping once instead of in the inner loop but this
        // problem finishes so fast i really can't care, talk to me in part B if it matters
        for x in clamp(xmin)..=clamp(xmax) {
            let x = (x - MIN_COORD) as usize;
            for y in clamp(ymin)..=clamp(ymax) {
                let y = (y - MIN_COORD) as usize;
                for z in clamp(zmin)..=clamp(zmax) {
                    let z = (z - MIN_COORD) as usize;
                    grid[x][y][z] = on;
                }
            }
        }
    }

    grid.iter()
        .flat_map(|x_seg| x_seg.iter())
        .flat_map(|y_seg| y_seg.iter())
        .filter(|b| **b)
        .count()
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

type SizeType = u128;

fn b_with_input(input: &str) -> SizeType {
    let ParseResult { lines } = parse(input);

    // invariant: this the collection of filled boxes that are completely full
    //      each box in this vector is completely disjoint from the other boxes
    let mut filled_boxes: Vec<FilledBox> = Vec::new();

    for Line {
        on,
        xmin,
        xmax,
        ymin,
        ymax,
        zmin,
        zmax,
    } in lines
    {
        let change_box = FilledBox {
            xmin,
            xmax,
            ymin,
            ymax,
            zmin,
            zmax,
        };

        let mut new_filled_boxes = Vec::new();

        while let Some(old_filled_box) = filled_boxes.pop() {
            if !old_filled_box.intersects(change_box) {
                new_filled_boxes.push(old_filled_box);
            } else if change_box.contains(old_filled_box) {
                // then do nothing, because we need to insert this at the end anyway
            } else {
                // then we need to subdivide the old box into pieces disjoint from the change box
                // and add all of those to the new_filled_boxes
                for box_piece in old_filled_box.subdivide(change_box) {
                    new_filled_boxes.push(box_piece);
                }
            }
        }

        if on {
            new_filled_boxes.push(change_box);
        }

        filled_boxes = new_filled_boxes;
    }

    filled_boxes.into_iter().map(|b| b.size()).sum()
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
struct FilledBox {
    // PRE: xmin <= xmax, ymin <= ymax, zmin <= zmax
    xmin: i32,
    xmax: i32,
    ymin: i32,
    ymax: i32,
    zmin: i32,
    zmax: i32,
}

// whether these intervals have any intersection
// note we assume the interval is closed (i.e. a..=b) here and throughout
#[inline(always)]
fn one_axis_intersect(a_min: i32, a_max: i32, b_min: i32, b_max: i32) -> bool {
    b_max >= a_min && a_max >= b_min
}

// whether the A (left params) fully contain B (right params)
#[inline(always)]
fn one_axis_a_contains_b(a_min: i32, a_max: i32, b_min: i32, b_max: i32) -> bool {
    a_min <= b_min && b_max <= a_max
}

impl FilledBox {
    fn maybe_new(
        xmin: i32,
        xmax: i32,
        ymin: i32,
        ymax: i32,
        zmin: i32,
        zmax: i32,
    ) -> Option<FilledBox> {
        if xmin > xmax || ymin > ymax || zmin > zmax {
            None
        } else {
            Some(FilledBox {
                xmin,
                xmax,
                ymin,
                ymax,
                zmin,
                zmax,
            })
        }
    }

    fn size(self) -> SizeType {
        let l = (self.xmax - self.xmin + 1) as SizeType;
        let w = (self.ymax - self.ymin + 1) as SizeType;
        let h = (self.zmax - self.zmin + 1) as SizeType;

        l * w * h
    }

    fn intersects(self, other: Self) -> bool {
        one_axis_intersect(self.xmin, self.xmax, other.xmin, other.xmax)
            && one_axis_intersect(self.ymin, self.ymax, other.ymin, other.ymax)
            && one_axis_intersect(self.zmin, self.zmax, other.zmin, other.zmax)
    }

    fn contains(self, other: Self) -> bool {
        one_axis_a_contains_b(self.xmin, self.xmax, other.xmin, other.xmax)
            && one_axis_a_contains_b(self.ymin, self.ymax, other.ymin, other.ymax)
            && one_axis_a_contains_b(self.zmin, self.zmax, other.zmin, other.zmax)
    }

    // subdivides self into a vec of filled boxes which are all disjoint from each other and from
    // the dividing param, and which in union give exactly the set difference (self - other)
    fn subdivide(self, other: Self) -> Vec<FilledBox> {
        // the "low x" piece；note that if this has no low_x piece then this will return None
        // and we filter out at the end
        let low_x = FilledBox::maybe_new(
            self.xmin,
            i32::min(self.xmax, other.xmin - 1),
            self.ymin,
            self.ymax,
            self.zmin,
            self.zmax,
        );

        // the "high x" piece
        let high_x = FilledBox::maybe_new(
            i32::max(self.xmin, other.xmax + 1),
            self.xmax,
            self.ymin,
            self.ymax,
            self.zmin,
            self.zmax,
        );

        // the rest will be in the x-intersection of the two cubes
        let xmin = i32::max(self.xmin, other.xmin);
        let xmax = i32::min(self.xmax, other.xmax);

        let low_y = FilledBox::maybe_new(
            xmin,
            xmax,
            self.ymin,
            i32::min(self.ymax, other.ymin - 1),
            self.zmin,
            self.zmax,
        );

        let high_y = FilledBox::maybe_new(
            xmin,
            xmax,
            i32::max(self.ymin, other.ymax + 1),
            self.ymax,
            self.zmin,
            self.zmax,
        );

        // the rest will be in the y-intersection of the two cubes
        let ymin = i32::max(self.ymin, other.ymin);
        let ymax = i32::min(self.ymax, other.ymax);

        let low_z = FilledBox::maybe_new(
            xmin,
            xmax,
            ymin,
            ymax,
            self.zmin,
            i32::min(self.zmax, other.zmin - 1),
        );

        let high_z = FilledBox::maybe_new(
            xmin,
            xmax,
            ymin,
            ymax,
            i32::max(self.zmin, other.zmax + 1),
            self.zmax,
        );

        [low_x, high_x, low_y, high_y, low_z, high_z]
            .iter()
            .copied()
            .flatten()
            .collect()
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
struct ParseResult {
    lines: Vec<Line>,
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
struct Line {
    on: bool,
    xmin: i32,
    xmax: i32,
    ymin: i32,
    ymax: i32,
    zmin: i32,
    zmax: i32,
}

fn parse(input: &str) -> ParseResult {
    fn parse_line(line: &str) -> Line {
        let (on, skip) = {
            if line.starts_with("on ") {
                (true, "on ".len())
            } else if line.starts_with("off ") {
                (false, "off ".len())
            } else {
                panic!("bad line: {}", line)
            }
        };

        let nums: Vec<i32> = line
            .slice(skip..)
            .replace("x=", "")
            .replace("y=", "")
            .replace("z=", "")
            .split(',')
            .flat_map(|tok| tok.split(".."))
            .map(|tok| tok.parse::<i32>().unwrap())
            .collect();

        assert_eq!(nums.len(), 6);

        Line {
            on,
            xmin: nums[0],
            xmax: nums[1],
            ymin: nums[2],
            ymax: nums[3],
            zmin: nums[4],
            zmax: nums[5],
        }
    }

    ParseResult {
        lines: input.lines().map(parse_line).collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SMALL_SAMPLE: &'static str = "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10";

    const SAMPLE: &'static str = "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682";

    const BIG_SAMPLE: &'static str = "on x=-5..47,y=-31..22,z=-19..33
on x=-44..5,y=-27..21,z=-14..35
on x=-49..-1,y=-11..42,z=-10..38
on x=-20..34,y=-40..6,z=-44..1
off x=26..39,y=40..50,z=-2..11
on x=-41..5,y=-41..6,z=-36..8
off x=-43..-33,y=-45..-28,z=7..25
on x=-33..15,y=-32..19,z=-34..11
off x=35..47,y=-46..-34,z=-11..5
on x=-14..36,y=-6..44,z=-16..29
on x=-57795..-6158,y=29564..72030,z=20435..90618
on x=36731..105352,y=-21140..28532,z=16094..90401
on x=30999..107136,y=-53464..15513,z=8553..71215
on x=13528..83982,y=-99403..-27377,z=-24141..23996
on x=-72682..-12347,y=18159..111354,z=7391..80950
on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
on x=-52752..22273,y=-49450..9096,z=54442..119054
on x=-29982..40483,y=-108474..-28371,z=-24328..38471
on x=-4958..62750,y=40422..118853,z=-7672..65583
on x=55694..108686,y=-43367..46958,z=-26781..48729
on x=-98497..-18186,y=-63569..3412,z=1232..88485
on x=-726..56291,y=-62629..13224,z=18033..85226
on x=-110886..-34664,y=-81338..-8658,z=8914..63723
on x=-55829..24974,y=-16897..54165,z=-121762..-28058
on x=-65152..-11147,y=22489..91432,z=-58782..1780
on x=-120100..-32970,y=-46592..27473,z=-11695..61039
on x=-18631..37533,y=-124565..-50804,z=-35667..28308
on x=-57817..18248,y=49321..117703,z=5745..55881
on x=14781..98692,y=-1341..70827,z=15753..70151
on x=-34419..55919,y=-19626..40991,z=39015..114138
on x=-60785..11593,y=-56135..2999,z=-95368..-26915
on x=-32178..58085,y=17647..101866,z=-91405..-8878
on x=-53655..12091,y=50097..105568,z=-75335..-4862
on x=-111166..-40997,y=-71714..2688,z=5609..50954
on x=-16602..70118,y=-98693..-44401,z=5197..76897
on x=16383..101554,y=4615..83635,z=-44907..18747
off x=-95822..-15171,y=-19987..48940,z=10804..104439
on x=-89813..-14614,y=16069..88491,z=-3297..45228
on x=41075..99376,y=-20427..49978,z=-52012..13762
on x=-21330..50085,y=-17944..62733,z=-112280..-30197
on x=-16478..35915,y=36008..118594,z=-7885..47086
off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
off x=2032..69770,y=-71013..4824,z=7471..94418
on x=43670..120875,y=-42068..12382,z=-24787..38892
off x=37514..111226,y=-45862..25743,z=-16714..54663
off x=25699..97951,y=-30668..59918,z=-15349..69697
off x=-44271..17935,y=-9516..60759,z=49131..112598
on x=-61695..-5813,y=40978..94975,z=8655..80240
off x=-101086..-9439,y=-7088..67543,z=33935..83858
off x=18020..114017,y=-48931..32606,z=21474..89843
off x=-77139..10506,y=-89994..-18797,z=-80..59318
off x=8476..79288,y=-75520..11602,z=-96624..-24783
on x=-47488..-1262,y=24338..100707,z=16292..72967
off x=-84341..13987,y=2429..92914,z=-90671..-1318
off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
off x=-27365..46395,y=31009..98017,z=15428..76570
off x=-70369..-16548,y=22648..78696,z=-1892..86821
on x=-53470..21291,y=-120233..-33476,z=-44150..38147
off x=-93533..-4276,y=-16170..68771,z=-104985..-24507";

    #[test]
    fn parse_test() {
        let actual = parse(
            "on x=-20..26,y=-36..17,z=-47..7
off x=-20..33,y=-21..23,z=-26..28",
        );

        let expected = ParseResult {
            lines: vec![
                Line {
                    on: true,
                    xmin: -20,
                    xmax: 26,
                    ymin: -36,
                    ymax: 17,
                    zmin: -47,
                    zmax: 7,
                },
                Line {
                    on: false,
                    xmin: -20,
                    xmax: 33,
                    ymin: -21,
                    ymax: 23,
                    zmin: -26,
                    zmax: 28,
                },
            ],
        };

        assert_eq!(actual, expected)
    }

    #[test]
    fn sample_a_small() {
        let actual = a_with_input(SMALL_SAMPLE);
        let expected = 39;

        assert_eq!(actual, expected);
    }

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 590784;

        assert_eq!(actual, expected);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(BIG_SAMPLE);
        let expected = 2758514936282235;

        assert_eq!(expected, actual);
    }
}
