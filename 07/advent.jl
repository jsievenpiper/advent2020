function parse(rawentry)
  fields = split.(split(rawentry), ":")
  dict = Dict()

  for field in fields
    dict[field[1]] = field[2]
  end

  dict
end

function validate(entry)
  haskey(entry, "byr") &&
  haskey(entry, "iyr") &&
  haskey(entry, "eyr") &&
  haskey(entry, "hgt") &&
  haskey(entry, "hcl") &&
  haskey(entry, "ecl") &&
  haskey(entry, "pid")
end

input = open(f -> read(f, String), "./input.txt")
entries = split(input, "\n\n")
parsed = parse.(entries)
valid = filter(x -> x, validate.(parsed))

println(length(valid))
