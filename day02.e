{
  ; forward X increases the horizontal position by X units.
  ; down X increases the depth by X units.
  ; up X decreases the depth by X units.

  ; Since ethel doesn't have nice string manipulation functions (yet),
  ; we rudely parse the input one byte and a time.

  ; Part 1

  val distance_product = fn(filename) {
    val instructions = read(filename)

    var depth = 0
    var horiz = 0

    var i = 0
    var c = ' '
    var n = 0

    while i < instructions.length() {
      c = instructions[i]
      n = 0

      ;; Read direction up to space.
      if c == 'u' then i = i + 3   ; up
      if c == 'd' then i = i + 5   ; down
      if c == 'f' then i = i + 8   ; forward

      ;; Read number up to newline.
      while (instructions[i] != 0x0a) {
        n = n * 10
        n = n + (instructions[i] - '0')
        i = i + 1
      }
      i = i + 1

      if c == 'u' then depth = depth - n
      if c == 'd' then depth = depth + n
      if c == 'f' then horiz = horiz + n
      print(horiz, "horiz,", depth, "depth")
    }

    horiz * depth
  }

  ; Part 2

  ; down X increases your aim by X units.
  ; up X decreases your aim by X units.
  ; forward X does two things:
  ;     It increases your horizontal position by X units.
  ;     It increases your depth by your aim multiplied by X.

  val aim_product = fn(filename) {
    val instructions = read(filename)

    var depth = 0
    var horiz = 0
    var aim = 0

    var i = 0
    var c = ' '
    var n = 0

    while i < instructions.length() {
      c = instructions[i]
      n = 0

      ;; Read direction up to space.
      if c == 'u' then i = i + 3   ; up
      if c == 'd' then i = i + 5   ; down
      if c == 'f' then i = i + 8   ; forward

      ;; Read number up to newline.
      while (instructions[i] != 0x0a) {
        n = n * 10
        n = n + (instructions[i] - '0')
        i = i + 1
      }
      i = i + 1

      if c == 'u' then aim = aim - n
      if c == 'd' then aim = aim + n
      if c == 'f' then {
        horiz = horiz + n
        depth = depth + aim * n
      }
      print(horiz, "horiz,", depth, "depth")
    }

    horiz * depth
  }


  print("Part 1: Example =", distance_product("day02.example.txt"))
  print("Part 1: Input = ", distance_product("day02.input.txt"))

  print("Part 2: Example =", aim_product("day02.example.txt"))
  print("Part 2: Input = ", aim_product("day02.input.txt"))
}
