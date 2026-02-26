// Enigma Cipher - Gleam
// Type-safe language running on the BEAM (Erlang VM)

import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleam/result

const fwd_i = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
const fwd_ii = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
const fwd_iii = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]

const bwd_i = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
const bwd_ii = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
const bwd_iii = [19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10]

const notches = [16, 4, 21]
const reflector_b = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]

pub type Enigma {
  Enigma(
    rotors: #(Int, Int, Int),
    offsets: #(Int, Int, Int),
    plugboard: List(Int),
  )
}

fn at(lst: List(Int), idx: Int) -> Int {
  case list.at(lst, idx) {
    Ok(v) -> v
    Error(_) -> 0
  }
}

fn get_fwd(r: Int) -> List(Int) {
  case r {
    0 -> fwd_i
    1 -> fwd_ii
    2 -> fwd_iii
    _ -> fwd_i
  }
}

fn get_bwd(r: Int) -> List(Int) {
  case r {
    0 -> bwd_i
    1 -> bwd_ii
    2 -> bwd_iii
    _ -> bwd_i
  }
}

fn get_notch(r: Int) -> Int {
  at(notches, r)
}

fn mod26(x: Int) -> Int {
  let m = x % 26
  case m < 0 {
    True -> m + 26
    False -> m
  }
}

fn make_plugboard(pairs: List(#(Int, Int))) -> List(Int) {
  let base = list.range(0, 25)
  list.fold(pairs, base, fn(pb, pair) {
    let #(a, b) = pair
    list.index_map(pb, fn(v, i) {
      case i == a {
        True -> b
        False -> case i == b {
          True -> a
          False -> v
        }
      }
    })
  })
}

fn step_rotors(rotors: #(Int, Int, Int), offsets: #(Int, Int, Int)) -> #(Int, Int, Int) {
  let #(r0, r1, r2) = rotors
  let #(o0, o1, o2) = offsets
  let mid = o1 == get_notch(r1)
  let new_o2 = mod26(o2 + 1)
  let at_notch2 = o2 == get_notch(r2)
  let new_o1 = case at_notch2 || mid {
    True -> mod26(o1 + 1)
    False -> o1
  }
  let new_o0 = case mid {
    True -> mod26(o0 + 1)
    False -> o0
  }
  #(new_o0, new_o1, new_o2)
}

fn pass_forward(rotor: Int, offset: Int, ch: Int) -> Int {
  let inp = mod26(ch + offset)
  let out = at(get_fwd(rotor), inp)
  mod26(out - offset)
}

fn pass_backward(rotor: Int, offset: Int, ch: Int) -> Int {
  let inp = mod26(ch + offset)
  let out = at(get_bwd(rotor), inp)
  mod26(out - offset)
}

fn encrypt_chars(enigma: Enigma, chars: List(Int), acc: List(Int)) -> List(Int) {
  case chars {
    [] -> list.reverse(acc)
    [ch, ..rest] -> {
      let #(r0, r1, r2) = enigma.rotors
      let new_offsets = step_rotors(enigma.rotors, enigma.offsets)
      let #(o0, o1, o2) = new_offsets
      let c = at(enigma.plugboard, ch)
      let c = pass_forward(r2, o2, c)
      let c = pass_forward(r1, o1, c)
      let c = pass_forward(r0, o0, c)
      let c = at(reflector_b, c)
      let c = pass_backward(r0, o0, c)
      let c = pass_backward(r1, o1, c)
      let c = pass_backward(r2, o2, c)
      let c = at(enigma.plugboard, c)
      encrypt_chars(
        Enigma(..enigma, offsets: new_offsets),
        rest,
        [c, ..acc],
      )
    }
  }
}

fn encrypt(rotors: #(Int, Int, Int), key: #(Int, Int, Int), pairs: List(#(Int, Int)), msg: String) -> String {
  let pb = make_plugboard(pairs)
  let enigma = Enigma(rotors: rotors, offsets: key, plugboard: pb)
  let chars = string.to_utf_codepoints(msg)
    |> list.map(fn(cp) { string.utf_codepoint_to_int(cp) - 65 })
  let result = encrypt_chars(enigma, chars, [])
  result
    |> list.map(fn(i) { case string.utf_codepoint(i + 65) {
      Ok(cp) -> string.from_utf_codepoints([cp])
      Error(_) -> ""
    }})
    |> string.concat
}

fn run_test(label: String, expected: String, actual: String) {
  let status = case expected == actual {
    True -> "PASS"
    False -> "FAIL"
  }
  io.println(status <> " " <> label <> ": " <> actual <> " (expected " <> expected <> ")")
}

pub fn main() {
  io.println("Enigma Cipher - Gleam")
  run_test("Test 1", "BDZGO",
    encrypt(#(0,1,2), #(0,0,0), [], "AAAAA"))
  run_test("Test 2", "ILBDAAMTAZ",
    encrypt(#(0,1,2), #(0,0,0), [], "HELLOWORLD"))
  run_test("Test 3", "BZHGNOCRRTCM",
    encrypt(#(0,1,2), #(0,0,0), [], "ATTACKATDAWN"))
  run_test("Test 4", "DLTBBQVPQV",
    encrypt(#(0,1,2), #(12,2,10), [], "HELLOWORLD"))
  run_test("Test 5", "KZHDFQYHXT",
    encrypt(#(2,0,1), #(0,0,0), [], "HELLOWORLD"))
  run_test("Test 6", "IKACBBMTBF",
    encrypt(#(0,1,2), #(0,0,0), [#(0,1), #(2,3), #(4,5)], "HELLOWORLD"))
}
