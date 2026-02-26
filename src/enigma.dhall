-- Enigma Cipher - Dhall
-- Programmable configuration language (total, no side effects)
-- Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
-- PeopleTec Inc. - Guinness World Record Attempt 2026

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall

let fwdI  = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
let fwdII = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
let fwdIII= [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
let bwdI  = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
let bwdII = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
let bwdIII= [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
let reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
let notches = [16, 4, 21]

let State = { r0 : Natural, r1 : Natural, r2 : Natural
            , o0 : Natural, o1 : Natural, o2 : Natural
            , n1 : Natural, n2 : Natural }

let TestVector = { rotors : Text, key : Text, input : Text, expected : Text }

let config = {
  title = "Wehrmacht Enigma I Configuration - Dhall",
  author = "PeopleTec Inc.",
  purpose = "Guinness World Record Attempt 2026",

  algorithm = {
    stepping = "double-stepping anomaly",
    rotors = "I-II-III",
    reflector = "B",
    signal_path = "plugboard -> R -> M -> L -> reflector -> L -> M -> R -> plugboard"
  },

  wirings = {
    fwd_I = fwdI,
    fwd_II = fwdII,
    fwd_III = fwdIII,
    bwd_I = bwdI,
    bwd_II = bwdII,
    bwd_III = bwdIII,
    reflector_B = reflector,
    notches = notches
  },

  test_vectors = [
    { rotors = "I-II-III", key = "AAA", input = "AAAAA", expected = "BDZGO" },
    { rotors = "I-II-III", key = "AAA", input = "HELLOWORLD", expected = "ILBDAAMTAZ" },
    { rotors = "I-II-III", key = "AAA", input = "ATTACKATDAWN", expected = "BZHGNOCRRTCM" },
    { rotors = "I-II-III", key = "MCK", input = "HELLOWORLD", expected = "DLTBBQVPQV" },
    { rotors = "III-I-II", key = "AAA", input = "HELLOWORLD", expected = "KZHDFQYHXT" }
  ] : List TestVector
}

in config
