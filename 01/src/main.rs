fn main() {
  let data = include_str!("input.txt")
    .split_whitespace()
    .filter_map(|line| line.parse::<u64>().ok())
    .collect::<Vec<u64>>();

  data
    .iter()
    .enumerate()
    .find_map(|(i, item)| {
      data
        .iter()
        .skip(i)
        .find(|possibility| 2020 == *item + **possibility)
        .map(|result| (*item, *result))
    })
    .and_then(|answer| {
      println!("{} + {} = 2020", answer.0, answer.1);
      println!("{} * {} = {}", answer.0, answer.1, answer.0 * answer.1);

      Some(())
    });
}
