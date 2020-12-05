const
  slopeX = 3
  slopeY = 1
  initialMapCapacity = 12_000 # Bigger than the input just to be lazy with reallocs.

var
  mapSlice: seq[bool] = newSeqOfCap[bool](initialMapCapacity)
  mapWidth: int = 0
  hits: int = 0

## Playing with iterators to mask the EOF loop here. I think there are more nim-idiomatic patterns.
iterator inputLines(path: string): string {.raises: [IOError].} =
  var input = open(path, fmRead)
  defer: close(input)

  try:
    while true:
      yield readLine input
  except EOFError:
    discard

## Returns the next relevant map column index based on `slopeX`. This is basically a ghetto barrel shifting strategy.
## You could also pre-allocate the pattern such that there's enough space to not need to rotate (i.e. a flat 2d map).
proc mapColumnIndex(): iterator(): int =
  return iterator(): int =
    var currentColumn: int = 0
    while true:
      yield currentColumn
      currentColumn = (currentColumn + slopeX) mod mapWidth

## Yields the next row until the bottom of the map has been reached. This is based on `slopeY`.
proc mapRowIndex(): iterator(): int =
  return iterator(): int =
    let lastRow: int = mapSlice.len div mapWidth
    var currentRow: int = 0

    while currentRow < lastRow:
      yield currentRow
      currentRow += slopeY

## This is basically an iterator `zip` implementation that assists in terminating the infinite barrel shifter by capping
## at the row visiting iterator, which will end when the height of the input is reached. This technically checks for
## both iterators to end, but really we'd only absolutely have to check for the one.
proc travelCoordinates(): iterator(): (int, int) =
  let
      x = mapColumnIndex()
      y = mapRowIndex()

  # nim is full of these weird `while true` loops. The iterator doesn't return "done" until the _next_ iteration loop.
  # So you have to throw away the return value. This is documented in the nim docs, but honestly seems super yucky.
  # https://nim-lang.org/docs/manual.html#iterators-and-the-for-statement-firstminusclass-iterators
  return iterator(): (int, int) =
    while true:
      let value = (x(), y())

      if x.finished or y.finished:
        break

      yield value

## Given some column index, return the values stored in it.
proc mapColumn(index: int): seq[bool] =
  let columnLength: int = mapSlice.len div mapWidth
  var current: int = index
  var column = newSeqOfCap[bool](columnLength)

  while current < mapSlice.len:
    column.add mapSlice[current]
    current += mapWidth

  column

try:
  for line in inputLines "./input.txt":
    mapWidth = line.len

    for character in line:
      mapSlice.add '#' == character

except IOError:
  echo "input file missing"

# Keep going until we've hit the bottom of the map.
let coodinates = travelCoordinates()

for x, y in coodinates():
  let column = mapColumn x

  if column[y]:
    inc hits

echo "Hits: ", hits
