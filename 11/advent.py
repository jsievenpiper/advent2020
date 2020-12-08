file = open("./input.txt", "r")
contents = file.read()
entries = map(lambda entry: entry.split("\n"), contents.split("\n\n"))
entries = map(lambda entry: { letter for line in entry for letter in line}, entries)
counts = map(lambda entry: len(entry), entries)

print(sum(counts))
