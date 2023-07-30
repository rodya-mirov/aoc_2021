use std::collections::HashSet;

fn input() -> String {
    std::fs::read_to_string("input/input_19.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> usize {
    let sensor_data = parse(input);
    let mapping_result = map_the_ocean(sensor_data);
    mapping_result.beacons.len()
}

struct MappingResult {
    beacons: HashSet<Pos>,
    sensor_positions: Vec<Pos>,
}

// gotta have at least this many exactly agreeing (post-transform)
// or the sensors aren't actually synced up
const ALIGNMENT_THRESHOLD: usize = 12;

fn map_the_ocean(mut sensor_data: Vec<Vec<Pos>>) -> MappingResult {
    // One assumption we're making is that if it's possible for sensor A and sensor B to overlap
    // (i.e. they have 12 compatible beacons) then they truly do overlap and those potentially-
    // equal beacons really are equal. The problem doesn't state this but I think it's the spirit.

    // Anyway we'll take sensor 0 as the absolute truth and branch out from there. When a sensor
    // is made to match sensor 0, we'll alter the rotation and translation of its coordinates.

    struct Transform {
        axes: [usize; 3],
        signs: [i32; 3],
        // applied last
        translate: [i32; 3],
    }

    impl Transform {
        fn apply(&self, pos: Pos) -> Pos {
            [
                pos[self.axes[0]] * self.signs[0] + self.translate[0],
                pos[self.axes[1]] * self.signs[1] + self.translate[1],
                pos[self.axes[2]] * self.signs[2] + self.translate[2],
            ]
        }
    }

    fn sub(a: Pos, b: Pos) -> Pos {
        [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
    }

    fn plus(a: Pos, b: Pos) -> Pos {
        [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
    }

    fn try_align(base: &[Pos], candidate: &[Pos]) -> Option<Transform> {
        // if alignment is possible, returns true and a transformation function to align the
        // candidate to the base
        // if alignment is not possible, returns false
        let axes = [0, 1, 2];
        let signs = [1, -1];

        for x_axis in axes {
            // meaning the post-transform x-axis will be this axis when untransformed
            for x_sign in signs {
                for y_axis in axes {
                    if y_axis == x_axis {
                        continue;
                    }
                    for y_sign in signs {
                        for z_axis in axes {
                            if z_axis == x_axis || z_axis == y_axis {
                                continue;
                            }
                            for z_sign in signs {
                                // PERF: this block is repeated a jillion times, so optimizing it
                                //       bears fruit. Or we could try to guess which pairs of sensor
                                //       are likely to line up, but that seems hard?
                                let cand_rotated: Vec<Pos> = candidate
                                    .iter()
                                    .map(|p| [x_sign * p[x_axis], y_sign * p[y_axis], z_sign * p[z_axis]])
                                    .collect();

                                // now that we picked a rotation, we still need alignment of at least 12 beacons
                                // so basically we'll pick a pair from each side and get a translation
                                // if that translation works 12 times, we're done
                                // otherwise move on to the next pair

                                // we find a base pair to get a translation for, then ensure that
                                // that translation works for at least 12 pairs across the two arrays
                                // PERF: if you use (a, b) to get a translation that works for (c, d)
                                //       then (c, d) would give you the same translation. Thus we
                                //       can ensure there is SOME base pair to give you that
                                //       translation where the coords are not in the last 11 items
                                //       so in our hunt we can skip the last 11 items on both sides
                                //          (saves about 60%)
                                for i in 0..cand_rotated.len() - (ALIGNMENT_THRESHOLD - 1) {
                                    for j in 0..base.len() - (ALIGNMENT_THRESHOLD - 1) {
                                        let translation = sub(base[j], cand_rotated[i]);
                                        assert_eq!(plus(cand_rotated[i], translation), base[j]);

                                        let found = base
                                            .iter()
                                            .copied()
                                            .filter(|p| cand_rotated.contains(&sub(*p, translation)))
                                            .count();

                                        if found >= ALIGNMENT_THRESHOLD {
                                            return Some(Transform {
                                                axes: [x_axis, y_axis, z_axis],
                                                signs: [x_sign, y_sign, z_sign],
                                                translate: translation,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        None
    }

    let mut aligned_sensors = HashSet::new();
    aligned_sensors.insert(0);

    let mut base_points = vec![0];

    let mut beacons: HashSet<Pos> = HashSet::new();
    for pos in sensor_data[0].iter() {
        beacons.insert(*pos);
    }

    let mut sensor_positions = vec![None; sensor_data.len()];
    sensor_positions[0] = Some([0, 0, 0]);

    while let Some(base_idx) = base_points.pop() {
        println!(
            "Considering {} as a base point against {} unknowns",
            base_idx,
            sensor_data.len() - aligned_sensors.len()
        );

        for cand_idx in 0..sensor_data.len() {
            if aligned_sensors.contains(&cand_idx) {
                continue;
            }

            let transform = try_align(&sensor_data[base_idx], &sensor_data[cand_idx]);

            if let Some(transform) = transform {
                println!("  Matched {} with {}", cand_idx, base_idx);
                aligned_sensors.insert(cand_idx);
                base_points.push(cand_idx);

                for pos in sensor_data.get_mut(cand_idx).unwrap().iter_mut() {
                    *pos = transform.apply(*pos);
                    beacons.insert(*pos);
                }

                let translation = transform.translate;
                sensor_positions[cand_idx] = Some(translation);
                println!("    Sensor {} is at {:?}", cand_idx, sensor_positions[cand_idx].unwrap())
            }
        }
    }

    MappingResult {
        beacons,
        sensor_positions: sensor_positions.into_iter().map(|p| p.unwrap()).collect(),
    }
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> i32 {
    let sensor_data = parse(input);
    let mapping_result = map_the_ocean(sensor_data);
    let sensor_positions = mapping_result.sensor_positions;

    fn dist(a: Pos, b: Pos) -> i32 {
        (a[0] - b[0]).abs() + (a[1] - b[1]).abs() + (a[2] - b[2]).abs()
    }

    let mut max_dist = i32::MIN;

    for i in 0..sensor_positions.len() {
        for j in i + 1..sensor_positions.len() {
            let d = dist(sensor_positions[i], sensor_positions[j]);
            max_dist = max_dist.max(d);
        }
    }

    max_dist
}

type Pos = [i32; 3];

fn parse(input: &str) -> Vec<Vec<Pos>> {
    let lines = input.lines().filter(|line| !line.is_empty());

    let mut all_sensors = Vec::new();

    let mut current_sensor = Vec::new();

    for line in lines {
        if line.starts_with("---") {
            if !current_sensor.is_empty() {
                all_sensors.push(current_sensor);
                current_sensor = Vec::new();
            }
        } else {
            let coords: Vec<i32> = line.split(',').map(|tok| tok.parse::<i32>().unwrap()).collect();
            assert_eq!(coords.len(), 3, "Positions should have 3 dimensions");
            let pos = [coords[0], coords[1], coords[2]];
            current_sensor.push(pos);
        }
    }

    if !current_sensor.is_empty() {
        all_sensors.push(current_sensor);
    }

    all_sensors
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str = "--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14";

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 79;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(SAMPLE);
        let expected = 3621;

        assert_eq!(expected, actual);
    }
}
