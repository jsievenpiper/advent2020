// This is just advent 01 with like, a couple small changes.
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
        .find_map(|item2| {
          data
            .iter()
            .skip(i + 1)
            .find(|possibility| 2020 == *item + *item2 + **possibility)
            .map(|item3| (item, item2, item3))
        })
    })
    .and_then(|answer| {
      println!("{} + {} + {} = 2020", answer.0, answer.1, answer.2);
      println!("{} * {} * {} = {}", answer.0, answer.1, answer.2, answer.0 * answer.1 * answer.2);

      Some(())
    });
}
