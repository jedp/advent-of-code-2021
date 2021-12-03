{ 
  ; You need to use the binary numbers in the diagnostic report to generate two
  ; new binary numbers (called the gamma rate and the epsilon rate). The power
  ; consumption can then be found by multiplying the gamma rate by the epsilon
  ; rate.

  ; Each bit in the gamma rate can be determined by finding the most common bit
  ; in the corresponding position of all numbers in the diagnostic report.

  val find_epsilon = fn(filename) {
    val report = read(filename)

    var bitcount = dict
    var cols = 0
    var i = 0
    var k = 0
    var c = ' '

    ; Count columns.
    while report[cols] != 0x0a { cols = cols + 1 }
    ; The newline
    cols = cols + 1

    ; Initialize dictionary elements.
    for k in 0..cols { bitcount[k] = 0 }

    ; Read and tally report bits.
    while i < report.length() {
      c = report[i]
      n = c - '0'
      k = i % cols

      if c != 0x0a then bitcount[k] = bitcount[k] + n
      i = i + 1
    }

    var epsilon = 0
    var gamma = 0
    var mid = i / (2 * cols)

    ; Ignore the newline column
    for k in 0..cols-2 {
      val x = cols - 1 - k
      if bitcount[k] > mid then gamma = gamma + (1 << (x-1)) else epsilon = epsilon + (1<< (x-1))
    }

    epsilon * gamma
  }

  ; Part 2. First attempt.
  ;
  ; Try to do this in most space-conserving manner possible, since
  ; Ethel's memory manager and GC are (cough) a work in progress.
  ;
  ; Read the data into a byte array, and winnow away "words" according
  ; to the rules
  ;
  ; E.g., starting with:
  ; 00000000  30 30 31 30 30 0a 31 31  31 31 30 0a 31 30 31 31  |00100.11110.1011|
  ; 00000010  30 0a 31 30 31 31 31 0a  31 30 31 30 31 0a 30 31  |0.10111.10101.01|
  ; 00000020  31 31 31 0a 30 30 31 31  31 0a 31 31 31 30 30 0a  |111.00111.11100.|
  ; 00000030  31 30 30 30 30 0a 31 31  30 30 31 0a 30 30 30 31  |10000.11001.0001|
  ; 00000040  30 0a 30 31 30 31 30 0a                           |0.01010.|
  ; Eliminate unless col 0 is 1
  ; 00000000  00 00 00 00 00 00 31 31  31 31 30 0a 31 30 31 31  |......11110.1011|
  ; 00000010  30 0a 31 30 31 31 31 0a  31 30 31 30 31 0a 00 00  |0.10111.10101...|
  ; 00000020  00 00 00 00 00 00 00 00  00 00 31 31 31 30 30 0a  |..........11100.|
  ; 00000030  31 30 30 30 30 0a 31 31  30 30 31 0a 00 00 00 00  |10000.11001.....|
  ; 00000040  00 00 00 00 00 00 00 00                           |........|
  ; Eliminate unless col 1 is 0
  ; 00000000  00 00 00 00 00 00 00 00  00 00 00 00 31 30 31 31  |............1011|
  ; 00000010  30 0a 31 30 31 31 31 0a  31 30 31 30 31 0a 00 00  |0.10111.10101...|
  ; 00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
  ; 00000030  31 30 30 30 30 0a 00 00  00 00 00 00 00 00 00 00  |10000...........|
  ; 00000040  00 00 00 00 00 00 00 00                           |........|
  ; ...
  ; Eliminate unless col 4 is 1
  ; 00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
  ; 00000010  00 00 31 30 31 31 31 0a  00 00 00 00 00 00 00 00  |..10111.........|
  ; 00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
  ; 00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
  ; 00000040  00 00 00 00 00 00 00 00                           |........|
  ;
  ; Then convert the last remaining binary word to decimal. Here, 10111 = 23.

  val count_bits_in_column = fn(report, cols, col, bit) {
    var i = col
    var count = 0

    while i < report.length() {
      if report[i] - '0' == bit then count = count + 1
      i = i + cols
    }

    print("col", col, "contains", count, bit, "bits")

    count
  }

  val eliminate_unless = fn(report, cols, col, bit) {
    print("Eliminate unless col", col, "is", bit)
    var i = 0
    var v = -1

    while i < report.length()-1 {
      v = report[i + col] - '0'
      if v != bit then {
        for q in 0..cols-1 {
          report[i + q] = 0x00
        }
      }
      i = i + cols
    }

    ;print(dump(report))

    nil
  }

  val find_rating = fn(filename, bit) {
    var report = read(filename)
    var cols = 0  ; number of cols in report, incl newline
    var k = 0     ; column iterator
    var c = ' '   ; char under analysis
    var i = 0     ; index for iterator
    var e = 0     ; elems in report
    val mid = cols / 2

    ; Count columns.
    while report[cols] != 0x0a { cols = cols + 1 }
    ; The newline
    cols = cols + 1

    e = report.length() / cols


    k = 0
    while k < cols {
      val x = count_bits_in_column(report, cols, k, bit)
      val y = count_bits_in_column(report, cols, k, (~bit & 1))

      if x + y < 2 then break

      if (bit) then {
        if y > x then {
          eliminate_unless(report, cols, k, (~bit & 1))
        } else {
          eliminate_unless(report, cols, k, bit)
        }
      } else {
        if y >= x then {
          eliminate_unless(report, cols, k, bit)
        } else {
          eliminate_unless(report, cols, k, (~bit & 1))
        }
      }

      k = k + 1
    }

    ; find the value
    ; Ignore the newline column
    var result = 0
    var x = 0
    for i in 0..report.length()-1 {
      if report[i] == '0' or report[i] == '1' then {
        for k in 0..cols-1 {
          x = cols - k - 2
          if report[i + k] == '1' then result = result + (1 << x)
        }
      }
      if result > 0 then break
    }


    result
  }

  ; Ok the first attempt wasn't as fast as I thought it would be.
  ; Second attempt :)

  val read_binary_numbers = fn(filename) {
    val report = read(filename)
    val l = list
    var n = 0
    var c = ' '
    for i in 0..report.length() - 1 {
      c = report[i] 
      if c == '1' then n = n * 2 + 1
      if c == '0' then n = n * 2
      if c == 0x0a then {
        print(i, n)
        l.append(n)
        n = 0
      }
    }
    print("Read numbers")
    l
  }

  val winnow = fn(values, bit, shift) {
    var ones = list
    var nones = list
    for k in 0..values.length()-1 {
      if values[k] & (1 << shift) then {
        ones.append(values[k])
      } else {
        nones.append(values[k])
      }
    }

    var result = list
    if ones.length()*2 >= values.length() then {
      if bit == 1 then result = ones else result = nones
    } else {
      if bit == 1 then result = nones else result = ones
    }

    print("Winnowed", shift, bit)
    result
  }

  val part2 = fn(filename) {
    val ns = read_binary_numbers(filename)
    var oxy = ns
    var co2 = ns

    ; Yes I'm hard-coding the width of the binary words here.
    ; I'm too lazy to copy the function from above that figures
    ; it out correctly. Pretend I did.
    for i in 11..0 {
      oxy = winnow(oxy, 1, i)
      if oxy.length() < 2 then break
    }

    print("Oxygen generator:", oxy[0])

    for i in 11..0 {
      co2 = winnow(co2, 0, i)
      if co2.length() < 2 then break
    }

    print("CO2 Scrubber:", co2[0])

    oxy[0] * co2[0]
  }

;  print("Example:", find_epsilon("day03.example.txt"))
;  print("Input:", find_epsilon("day03.input.txt"))

  print("Example:", part2("day03.example.txt"))
  print("Part 2:", part2("day03.input.txt"))
}
