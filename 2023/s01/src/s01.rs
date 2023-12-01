use regex::Regex;

fn find_digits(s: &str) -> (u32, u32) {
    let mut it = s.chars().filter_map(|c| c.to_digit(10));
    let first = it.next().unwrap();
    let second = it.fold(first, |_, c| c);
    (first, second)
}

fn glue_digits((first, second): (u32, u32)) -> u32 {
    first * 10 + second
}

fn task1(input: Vec<String>) -> String {
    input
        .iter()
        .map(|s| find_digits(s))
        .map(glue_digits)
        .sum::<u32>()
        .to_string()
}

fn first_and_last<'a, It>(mut it: It) -> (String, String)
where
    It: Iterator<Item = String>,
    // It: Iterator<Item = &'a str>,
{
    let first = it.next().unwrap();
    let last = it.last().unwrap_or(first.clone());
    (first, last)
}

fn to_number(s: &str) -> u32 {
    match s.parse() {
        Ok(n) => n,
        Err(_) => match s {
            "one" => 1,
            "two" => 2,
            "three" => 3,
            "four" => 4,
            "five" => 5,
            "six" => 6,
            "seven" => 7,
            "eight" => 8,
            "nine" => 9,
            _ => panic!("Invalid number: {}", s),
        },
    }
}

fn task2(input: Vec<String>) -> String {
    let re = Regex::new(r"[1-9]|one|two|three|four|five|six|seven|eight|nine").unwrap();
    input
        .iter()
        .map(|s| {
            s.replace("oneight", "eoneeight")
                .replace("threight", "threeeight")
                .replace("fiveight", "fiveeight")
                .replace("nineight", "nineeight")
                .replace("twone", "twoone")
                .replace("eightwo", "eighttwo")
                .replace("eighthree", "eightthree")
        })
        .map(|s| {
            first_and_last(
                re.captures_iter(&s)
                    .map(|m| m.get(0).unwrap().as_str().to_owned()),
            )
        })
        .map(|(f, s)| (to_number(&f), to_number(&s)))
        .map(glue_digits)
        .sum::<u32>()
        .to_string()
}

fn parser(input: &str) -> Vec<String> {
    input.lines().map(|x| x.to_string()).collect()
}

aoc2023::make_main!(task1, task2, parser: parser);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_finds_single_digit() {
        for i in 1..10 {
            let s = i.to_string();
            let r = task2(vec![s]);
            assert_eq!(r.parse::<u32>().unwrap(), i * 10 + i);
        }
    }

    #[test]
    fn test_finds_single_digit_spelled_out() {
        assert_eq!(task2(vec!["one".to_string()]), "11");
        assert_eq!(task2(vec!["two".to_string()]), "22");
        assert_eq!(task2(vec!["three".to_string()]), "33");
        assert_eq!(task2(vec!["four".to_string()]), "44");
        assert_eq!(task2(vec!["five".to_string()]), "55");
        assert_eq!(task2(vec!["six".to_string()]), "66");
        assert_eq!(task2(vec!["seven".to_string()]), "77");
        assert_eq!(task2(vec!["eight".to_string()]), "88");
        assert_eq!(task2(vec!["nine".to_string()]), "99");
    }

    #[test]
    fn test_finds_overlapping() {
        assert_eq!(task2(vec!["oneight".to_string()]), "18");
    }
}
