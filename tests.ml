let test_01 = {|
  inp x
  mul x -1
|}

let test_02 = {|
  inp x
  inp y
  mul x 3
  eql x y
|}

let test_03 = {|
  inp w
  add z w
  mod z 2
  div w 2
  add y w
  mod y 2
  div w 2
  add x w
  mod x 2
  div w 2
  mod w 2
|}

let test_04 = Alu.Utils.read_file_into_string "aoc_2021.txt"

let tests = [
  test_01;
  test_02;
  test_03;
  test_04;
]
