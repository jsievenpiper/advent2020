using Distributed

currentCpuCount = nprocs()
desiredCpuCount = max(1, length(Sys.cpu_info()) - 1)

addprocs(desiredCpuCount - currentCpuCount)

@everywhere function shakespeare(input)
  split(read(pipeline(`echo $input`, `shakespeare run program.spl`), String))
end

input = open(f -> read(f, String), "./input.txt")
lines = split(input)
values = pmap(shakespeare, lines)
values = map(value -> parse(Int, value[3]), values)
values = sort(values)

for i in 8:(length(values) - 8)
  if values[i] + 1 != values[i + 1]
    println(values[i] + 1)
  end
end
