fn input() -> String {
    std::fs::read_to_string("input/input_20.txt").expect("Should be able to read the file")
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> usize {
    let (algorithm, mut image) = parse(input);

    image = enhance(&algorithm, &image);

    image = enhance(&algorithm, &image);

    if image.infinite_sign {
        panic!("Infinite magnitude!!!1");
    }

    image
        .known_pixels
        .iter()
        .map(|row| row.iter().copied().filter(|b| *b).count())
        .sum()
}

fn to_binary(pixels: [bool; 9]) -> usize {
    let mut binary = 0;
    let mut pow = 1;
    for i in (0..9).rev() {
        if pixels[i] {
            binary += pow;
        }
        pow <<= 1;
    }
    binary
}

fn read_pixel_from_image(x: isize, y: isize, image: &Image) -> bool {
    if x < 0 || y < 0 {
        return image.infinite_sign;
    }

    let old_x = x as usize;
    let old_y = y as usize;

    if old_y >= image.known_pixels.len() {
        return image.infinite_sign;
    }
    let row = &image.known_pixels[old_y];
    if old_x >= row.len() {
        return image.infinite_sign;
    }

    row[old_x]
}

fn read_grid_from_image(x: isize, y: isize, image: &Image) -> [bool; 9] {
    let p = |x, y| read_pixel_from_image(x, y, image);

    [
        p(x - 1, y - 1),
        p(x, y - 1),
        p(x + 1, y - 1),
        p(x - 1, y),
        p(x, y),
        p(x + 1, y),
        p(x - 1, y + 1),
        p(x, y + 1),
        p(x + 1, y + 1),
    ]
}

fn enhance(algorithm: &[bool; 512], image: &Image) -> Image {
    // once you're two pixels away from the original image you're just matching on
    // their default state; we capture that as the new default state of this image, and
    // don't need to track it separately
    //
    // it's not wrong to increase this (you still get the right answer) but it affects
    // the performance of part B in a significant way
    let buffer_size: isize = 1;

    let h = image.known_pixels.len() + 2 * (buffer_size as usize);
    let w = image.known_pixels[0].len() + 2 * (buffer_size as usize);

    let mut rows = Vec::with_capacity(h);

    for y in 0..h as isize {
        let mut row = Vec::with_capacity(w);

        for x in 0..w as isize {
            let old_x = x - buffer_size;
            let old_y = y - buffer_size;
            let local = read_grid_from_image(old_x, old_y, image);
            let bin_rep = to_binary(local);
            let new_pixel = algorithm[bin_rep];
            row.push(new_pixel);
        }

        rows.push(row);
    }

    let new_infinite_sign = if image.infinite_sign {
        // then the "very far away" pixels are all full on the original image
        // so the new "very far away" pixels are all whatever the algorithm does to a full image
        algorithm[511]
    } else {
        // and likewise here, if the "very far away" pixels are all empty
        // then the new "very far away" pixels are all whatever the algorithm does to an empty image
        algorithm[0]
    };

    Image {
        known_pixels: rows,
        infinite_sign: new_infinite_sign,
    }
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> usize {
    let (algorithm, mut image) = parse(input);

    for _ in 0..50 {
        image = enhance(&algorithm, &image);
    }

    if image.infinite_sign {
        panic!("Infinite magnitude!!!1");
    }

    image
        .known_pixels
        .iter()
        .map(|row| row.iter().copied().filter(|b| *b).count())
        .sum()
}

struct Image {
    known_pixels: Vec<Vec<bool>>,
    infinite_sign: bool,
}

fn parse(input: &str) -> ([bool; 512], Image) {
    let mut lines = input.lines();

    fn b(c: char) -> bool {
        match c {
            '.' => false,
            '#' => true,
            _ => panic!("Bad input {}", c),
        }
    }

    fn bline(s: &str) -> Vec<bool> {
        s.chars().map(b).collect()
    }

    let algo: Vec<bool> = bline(lines.next().unwrap());
    assert_eq!(algo.len(), 512);
    let algo: [bool; 512] = algo.try_into().unwrap();

    assert_eq!(lines.next(), Some(""));

    let mut rows = Vec::new();

    for line in lines {
        let row = bline(line);
        rows.push(row);
    }

    let image = Image {
        known_pixels: rows,
        infinite_sign: false,
    };

    (algo, image)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &'static str =
        "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###";

    #[test]
    fn sample_bin() {
        let actual = to_binary([false, false, false, true, false, false, false, true, false]);
        let expected = 34;

        assert_eq!(actual, expected);
    }

    #[test]
    fn sample_bin2() {
        let actual = to_binary([true; 9]);
        let expected = 511;

        assert_eq!(actual, expected);
    }

    #[test]
    fn sample_a() {
        let actual = a_with_input(SAMPLE);
        let expected = 35;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_b() {
        let actual = b_with_input(SAMPLE);
        let expected = 3351;

        assert_eq!(expected, actual);
    }
}
