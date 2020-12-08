def count_unique(entry):
  unique = set(entry[0])

  for line in entry[1:]:
    if 0 != len(line):
      unique = unique & line

  return len(unique)

file = open("./input.txt", "r")
contents = file.read()
entries = map(lambda entry: entry.split("\n"), contents.split("\n\n"))
entries = map(lambda entry: map(lambda line: {x for x in line}, entry), entries)
entries = map(count_unique, entries)

print(sum(entries))
