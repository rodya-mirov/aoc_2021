fn input() -> String {
    std::fs::read_to_string("input/input_16.txt").expect("Should be able to read the file")
}

#[derive(Copy, Clone, Debug, Hash, Eq, Ord, PartialOrd, PartialEq)]
enum Bit {
    Zero,
    One,
}

impl Bit {
    fn as_u64(self) -> u64 {
        match self {
            Zero => 0,
            One => 1,
        }
    }
}

use Bit::*;

struct Packet {
    version: u8,
    type_id: u8,
    length_bits: u64,
    contents: PacketContents,
}

enum PacketContents {
    Literal(u64),
    Op { children: Vec<Packet> },
}

pub fn a() -> String {
    let contents = input();

    let val = a_with_input(&contents);

    val.to_string()
}

fn a_with_input(input: &str) -> u64 {
    let mut bits_iter = to_bits(input.chars());
    let bits = &mut bits_iter;

    let packet = parse_packet(bits);

    let leftover_bits = bits.count();

    assert!(leftover_bits < 8, "Shouldn't have too much leftover crud; got {}", leftover_bits);

    fn sum_versions(packet: &Packet) -> u64 {
        let mut total = packet.version as u64;

        match &packet.contents {
            PacketContents::Literal(_) => {}
            PacketContents::Op { children } => {
                for child in children.iter() {
                    total += sum_versions(child);
                }
            }
        }

        total
    }

    sum_versions(&packet)
}

fn parse_packet(bits: &mut impl Iterator<Item = Bit>) -> Packet {
    let version = read_uint(3, bits) as u8;
    let type_id = read_uint(3, bits) as u8;

    let (seen_bits_child, contents) = match type_id {
        4 => {
            let (seen_bits, val) = read_literal(bits);
            (seen_bits, PacketContents::Literal(val))
        }
        _ => {
            let length_type_id = bits.next().unwrap();
            match length_type_id {
                Zero => {
                    let expected_child_size = read_uint(15, bits);
                    // Next expected_child_size bits are the sub-packets
                    let mut children = Vec::new();

                    let mut running_size = 0;

                    while running_size < expected_child_size {
                        let child = parse_packet(bits);
                        running_size += child.length_bits;
                        children.push(child);
                    }

                    if running_size > expected_child_size {
                        panic!(
                            "Bad input -- needed to get exactly {} bits of children, but got {}",
                            expected_child_size, running_size
                        );
                    }

                    (1 + 15 + running_size, PacketContents::Op { children })
                }
                One => {
                    // Next 11 bits are the number of sub-packets
                    let num_subs = read_uint(11, bits);
                    let mut children = Vec::with_capacity(num_subs as usize);
                    let mut child_bits = 0;

                    for _ in 0..num_subs {
                        let child = parse_packet(bits);
                        child_bits += child.length_bits;
                        children.push(child);
                    }

                    (1 + 11 + child_bits, PacketContents::Op { children })
                }
            }
        }
    };

    Packet {
        version,
        type_id,
        length_bits: seen_bits_child + 6,
        contents,
    }
}

fn read_uint(n: usize, iter: &mut impl Iterator<Item = Bit>) -> u64 {
    assert!(n <= 63);

    let mut pow = 1;
    for _ in 0..n - 1 {
        pow <<= 1;
    }
    let mut out = 0;
    for _ in 0..n {
        out += pow * iter.next().unwrap().as_u64();
        pow >>= 1;
    }
    out
}

fn read_literal(iter: &mut impl Iterator<Item = Bit>) -> (u64, u64) {
    let mut out = 0;
    let mut seen_bits = 0;

    let mut done = false;
    while !done {
        let next = read_uint(5, iter);
        if next & 16 == 0 {
            done = true;
        }
        out = (out * 16) + (next & 15);
        seen_bits += 5;
    }

    (seen_bits, out)
}

fn to_bits(chars: impl Iterator<Item = char>) -> impl Iterator<Item = Bit> {
    fn bits(c: char) -> [Bit; 4] {
        match c {
            '0' => [Zero, Zero, Zero, Zero],
            '1' => [Zero, Zero, Zero, One],
            '2' => [Zero, Zero, One, Zero],
            '3' => [Zero, Zero, One, One],

            '4' => [Zero, One, Zero, Zero],
            '5' => [Zero, One, Zero, One],
            '6' => [Zero, One, One, Zero],
            '7' => [Zero, One, One, One],

            '8' => [One, Zero, Zero, Zero],
            '9' => [One, Zero, Zero, One],
            'A' => [One, Zero, One, Zero],
            'B' => [One, Zero, One, One],

            'C' => [One, One, Zero, Zero],
            'D' => [One, One, Zero, One],
            'E' => [One, One, One, Zero],
            'F' => [One, One, One, One],
            _ => unimplemented!("Bad input: {}", c),
        }
    }

    chars.flat_map(|c| bits(c).into_iter())
}

pub fn b() -> String {
    let contents = input();

    let val = b_with_input(&contents);

    val.to_string()
}

fn b_with_input(input: &str) -> u64 {
    let mut bits_iter = to_bits(input.chars());
    let bits = &mut bits_iter;

    let packet = parse_packet(bits);

    let leftover_bits = bits.count();

    assert!(leftover_bits < 8, "Shouldn't have too much leftover crud; got {}", leftover_bits);

    fn eval(packet: &Packet) -> u64 {
        match &packet.contents {
            PacketContents::Literal(v) => *v,
            PacketContents::Op { children } => {
                assert!(!children.is_empty());
                match packet.type_id {
                    0 => {
                        let mut out = 0;
                        for c in children.iter() {
                            out += eval(c);
                        }
                        out
                    }
                    1 => {
                        let mut out = 1;
                        for c in children.iter() {
                            out *= eval(c);
                        }
                        out
                    }
                    2 => {
                        let mut out = u64::MAX;
                        for c in children.iter() {
                            out = out.min(eval(c));
                        }
                        out
                    }
                    3 => {
                        let mut out = u64::MIN;
                        for c in children.iter() {
                            out = out.max(eval(c));
                        }
                        out
                    }
                    4 => unreachable!("This is a literal"),
                    5 => {
                        assert_eq!(children.len(), 2);
                        let a = eval(&children[0]);
                        let b = eval(&children[1]);

                        if a > b {
                            1
                        } else {
                            0
                        }
                    }
                    6 => {
                        assert_eq!(children.len(), 2);
                        let a = eval(&children[0]);
                        let b = eval(&children[1]);

                        if a < b {
                            1
                        } else {
                            0
                        }
                    }
                    7 => {
                        assert_eq!(children.len(), 2);
                        let a = eval(&children[0]);
                        let b = eval(&children[1]);

                        if a == b {
                            1
                        } else {
                            0
                        }
                    }
                    _ => unimplemented!("Type ID unsupported {}", packet.type_id),
                }
            }
        }
    }

    eval(&packet)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample_a_basic_1() {
        let actual = a_with_input("D2FE28");
        let expected = 6;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a_basic_2() {
        let actual = a_with_input("38006F45291200");
        let expected = 1 + 6 + 2;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a_basic_3() {
        let actual = a_with_input("EE00D40C823060");
        let expected = 7 + 2 + 4 + 1;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a1() {
        let actual = a_with_input("8A004A801A8002F478");
        let expected = 16;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a2() {
        let actual = a_with_input("620080001611562C8802118E34");
        let expected = 12;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a3() {
        let actual = a_with_input("C0015000016115A2E0802F182340");
        let expected = 23;

        assert_eq!(expected, actual);
    }

    #[test]
    fn sample_a4() {
        let actual = a_with_input("A0016C880162017C3686B18A3D4780");
        let expected = 31;

        assert_eq!(expected, actual);
    }

    fn b_test(input: &str, exp: u64) {
        let actual = b_with_input(input);
        assert_eq!(exp, actual, "Failed on input {}", input);
    }

    #[test]
    fn sample_b() {
        for (sample, val) in [
            ("C200B40A82", 3),
            ("04005AC33890", 54),
            ("880086C3E88112", 7),
            ("CE00C43D881120", 9),
            ("D8005AC2A8F0", 1),
            ("F600BC2D8F", 0),
            ("9C005AC2F8F0", 0),
            ("9C0141080250320F1802104A08", 1),
        ] {
            b_test(sample, val);
        }
    }
}
