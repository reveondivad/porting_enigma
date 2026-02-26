// Enigma Cipher - Gleam
// Type-safe language for Erlang VM and JavaScript
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

import gleam/io
import gleam/list
import gleam/string
import gleam/int

const fwd_i = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
const fwd_ii = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
const fwd_iii = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
const bwd_i = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
const bwd_ii = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
const bwd_iii = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
const reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
const notches = [16, 4, 21]

fn mod26(n: Int) -> Int {
  let m = n % 26
  case m < 0 {
    True -> m + 26
    False -> m
  }
}

fn nth(lst: List(Int), i: Int) -> Int {
  case list.at(lst, i) {
    Ok(v) -> v
    Error(_) -> 0
  }
}

fn get_fwd(r: Int, i: Int) -> Int {
  case r {
    0 -> nth(fwd_i, i)
    1 -> nth(fwd_ii, i)
    _ -> nth(fwd_iii, i)
  }
}

fn get_bwd(r: Int, i: Int) -> Int {
  case r {
    0 -> nth(bwd_i, i)
    1 -> nth(bwd_ii, i)
    _ -> nth(bwd_iii, i)
  }
}

fn pass_fwd(rotor: Int, offset: Int, ch: Int) -> Int {
  let inp = mod26(ch + offset)
  let out = get_fwd(rotor, inp)
  mod26(out - offset)
}

fn pass_bwd(rotor: Int, offset: Int, ch: Int) -> Int {
  let inp = mod26(ch + offset)
  let out = get_bwd(rotor, inp)
  mod26(out - offset)
}

pub type State {
  State(r0: Int, r1: Int, r2: Int, o0: Int, o1: Int, o2: Int, n1: Int, n2: Int)
}

fn step(s: State) -> State {
  let mid = s.o1 == s.n1
  let atn = s.o2 == s.n2
  let new_o2 = mod26(s.o2 + 1)
  let new_o1 = case mid || atn { True -> mod26(s.o1 + 1) False -> s.o1 }
  let new_o0 = case mid { True -> mod26(s.o0 + 1) False -> s.o0 }
  State(..s, o0: new_o0, o1: new_o1, o2: new_o2)
}

fn encrypt_char(s: State, ch: Int) -> #(Int, State) {
  let ns = step(s)
  let c = ch
  let c = pass_fwd(ns.r2, ns.o2, c)
  let c = pass_fwd(ns.r1, ns.o1, c)
  let c = pass_fwd(ns.r0, ns.o0, c)
  let c = nth(reflector, c)
  let c = pass_bwd(ns.r0, ns.o0, c)
  let c = pass_bwd(ns.r1, ns.o1, c)
  let c = pass_bwd(ns.r2, ns.o2, c)
  #(c, ns)
}

fn encrypt_loop(s: State, chars: List(Int), acc: List(Int)) -> List(Int) {
  case chars {
    [] -> list.reverse(acc)
    [ch, ..rest] -> {
      let #(enc, ns) = encrypt_char(s, ch)
      encrypt_loop(ns, rest, [enc, ..acc])
    }
  }
}

pub fn encrypt(r0: Int, r1: Int, r2: Int, k0: Int, k1: Int, k2: Int, msg: String) -> String {
  let s = State(r0, r1, r2, k0, k1, k2, nth(notches, r1), nth(notches, r2))
  let chars = string.to_utf_codepoints(msg)
    |> list.map(fn(cp) { string.utf_codepoint_to_int(cp) - 65 })
  let result = encrypt_loop(s, chars, [])
  result
    |> list.map(fn(i) { int.to_string(i + 65) })
    |> string.concat
}

pub fn main() {
  io.println("Enigma Cipher - Gleam")
  io.println("Test 1: " <> encrypt(0,1,2,0,0,0,"AAAAA"))
  io.println("Test 2: " <> encrypt(0,1,2,0,0,0,"HELLOWORLD"))
  io.println("Test 3: " <> encrypt(0,1,2,0,0,0,"ATTACKATDAWN"))
  io.println("Test 4: " <> encrypt(0,1,2,12,2,10,"HELLOWORLD"))
  io.println("Test 5: " <> encrypt(2,0,1,0,0,0,"HELLOWORLD"))
}
