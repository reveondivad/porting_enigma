/* Enigma Cipher - ReScript
   Type-safe language compiling to readable JavaScript
   Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
   PeopleTec Inc. - Guinness World Record Attempt 2026 */

let fwdI  = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
let fwdII = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
let fwdIII= [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
let bwdI  = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
let bwdII = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
let bwdIII= [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
let reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
let notches = [16, 4, 21]

let mod26 = n => {
  let m = mod(n, 26)
  m < 0 ? m + 26 : m
}

let getFwd = (r, i) => {
  switch r {
  | 0 => fwdI[i]
  | 1 => fwdII[i]
  | _ => fwdIII[i]
  }
}

let getBwd = (r, i) => {
  switch r {
  | 0 => bwdI[i]
  | 1 => bwdII[i]
  | _ => bwdIII[i]
  }
}

let passFwd = (rotor, offset, ch) => {
  let inp = mod26(ch + offset)
  let out = getFwd(rotor, inp)
  mod26(out - offset)
}

let passBwd = (rotor, offset, ch) => {
  let inp = mod26(ch + offset)
  let out = getBwd(rotor, inp)
  mod26(out - offset)
}

type state = {
  r: array<int>,
  o: array<int>,
  n1: int,
  n2: int,
}

let makeEnigma = (r0, r1, r2, k0, k1, k2) => {
  r: [r0, r1, r2],
  o: [k0, k1, k2],
  n1: notches[r1],
  n2: notches[r2],
}

let step = s => {
  if s.o[1] == s.n1 {
    s.o[1] = mod26(s.o[1] + 1)
    s.o[0] = mod26(s.o[0] + 1)
  } else if s.o[2] == s.n2 {
    s.o[1] = mod26(s.o[1] + 1)
  }
  s.o[2] = mod26(s.o[2] + 1)
}

let pressKey = (s, ch) => {
  step(s)
  let c = ref(ch)
  c := passFwd(s.r[2], s.o[2], c.contents)
  c := passFwd(s.r[1], s.o[1], c.contents)
  c := passFwd(s.r[0], s.o[0], c.contents)
  c := reflector[c.contents]
  c := passBwd(s.r[0], s.o[0], c.contents)
  c := passBwd(s.r[1], s.o[1], c.contents)
  c := passBwd(s.r[2], s.o[2], c.contents)
  c.contents
}

let encrypt = (r0, r1, r2, k0, k1, k2, msg) => {
  let s = makeEnigma(r0, r1, r2, k0, k1, k2)
  let result = ref("")
  for i in 0 to String.length(msg) - 1 {
    let ch = Char.code(String.get(msg, i)) - 65
    let enc = pressKey(s, ch)
    result := result.contents ++ String.make(1, Char.chr(enc + 65))
  }
  result.contents
}

Js.log("Enigma Cipher - ReScript")
Js.log("Test 1: " ++ encrypt(0,1,2,0,0,0,"AAAAA") ++ " expected BDZGO")
Js.log("Test 2: " ++ encrypt(0,1,2,0,0,0,"HELLOWORLD") ++ " expected ILBDAAMTAZ")
Js.log("Test 3: " ++ encrypt(0,1,2,0,0,0,"ATTACKATDAWN") ++ " expected BZHGNOCRRTCM")
Js.log("Test 4: " ++ encrypt(0,1,2,12,2,10,"HELLOWORLD") ++ " expected DLTBBQVPQV")
Js.log("Test 5: " ++ encrypt(2,0,1,0,0,0,"HELLOWORLD") ++ " expected KZHDFQYHXT")
