use std::env;
use std::fmt::{Display, Formatter, Write};
use std::time::Instant;

mod day1;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum Side {
    A,
    B,
}

impl Display for Side {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Side::A => f.write_char('a'),
            Side::B => f.write_char('b'),
        }
    }
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        Err(
            "Usage: [run] [problemnumber] [subcase] ; eg:\n\tcargo run --release -- 1 a"
                .to_string(),
        )
    } else {
        let a: i32 = args[1]
            .parse::<i32>()
            .map_err(|s| format!("Cannot parse argument '{}' as int", s))?;
        let b: Side = match args[2].as_str() {
            "a" => Ok(Side::A),
            "b" => Ok(Side::B),
            _err => Err(format!(
                "Cannot parse argument '{}' as subcase; should be 'a' or 'b'",
                args[0].as_str()
            )),
        }?;

        let start = Instant::now();

        let out: String = match (a, b) {
            (1, Side::A) => Ok(day1::a().to_string()),
            (1, Side::B) => Ok(day1::b().to_string()),
            (day, side) => Err(format!("Day {}, side {} is not yet supported", day, side)),
        }?;

        let elapsed = start.elapsed();

        println!("Day {} -- {}: {}", a, b, out);
        println!("Took {0:3} ms", elapsed.as_secs_f32() * 1000.0);

        Ok(())
    }
}