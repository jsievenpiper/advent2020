function entrytize(rawentry)
  fields = split.(split(rawentry), ":")
  dict = Dict{String, String}()

  for field in fields
    dict[field[1]] = field[2]
  end

  dict
end

function validatenumber(num, min, max)
  parsed = parse(Int, num, base = 10)
  parsed >= min && parsed <= max
end

function validateheight(height)
  value = String(collect(Iterators.takewhile(isdigit, height)))
  unit = String(collect(Iterators.dropwhile(isdigit, height)))

  if unit == "cm"
    return validatenumber(value, 150, 193)
  elseif unit == "in"
    return validatenumber(value, 59, 76)
  end

  false
end

function validatehaircolor(color)
  match(r"^#[a-fA-F0-9]{6}$", color) !== nothing
end

function validateeyecolor(color)
  color in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
end

function validate(entry)
  validatenumber(get(entry, "byr", "0"), 1920, 2002) &&
  validatenumber(get(entry, "iyr", "0"), 2010, 2020) &&
  validatenumber(get(entry, "eyr", "0"), 2020, 2030) &&
  validateheight(get(entry, "hgt", "")) &&
  validatehaircolor(get(entry, "hcl", "")) &&
  validateeyecolor(get(entry, "ecl", "")) &&
  9 == length(get(entry, "pid", ""))
end

input = open(f -> read(f, String), "./input.txt")
entries = split(input, "\n\n")
parsed = entrytize.(entries)
valid = filter(x -> x, validate.(parsed))

println(length(valid))
