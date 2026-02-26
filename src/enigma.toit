// Enigma Cipher - Toit
// IoT-focused language with hot-reloading
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

FWD ::= #[
  #[4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
  #[0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
  #[1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14],
]
BWD ::= #[
  #[20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
  #[0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
  #[19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12],
]
REF ::= #[24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
NOTCH ::= #[16, 4, 21]

mod26 n/int -> int:
  m := n % 26
  if m < 0: m += 26
  return m

class Enigma:
  rotors/List
  offsets/List
  notches/List
  pb/List

  constructor .rotors .offsets .notches .pb:

  static create r1/int r2/int r3/int k1/int k2/int k3/int -> Enigma:
    ri := [r1-1, r2-1, r3-1]
    pb := List 26: it
    return Enigma ri [k1, k2, k3] [NOTCH[ri[0]], NOTCH[ri[1]], NOTCH[ri[2]]] pb

  step:
    if offsets[1] == notches[1]:
      offsets[1] = mod26 (offsets[1] + 1)
      offsets[0] = mod26 (offsets[0] + 1)
    else if offsets[2] == notches[2]:
      offsets[1] = mod26 (offsets[1] + 1)
    offsets[2] = mod26 (offsets[2] + 1)

  fwd-pass slot/int idx/int -> int:
    contact := mod26 (idx + offsets[slot])
    out := FWD[rotors[slot]][contact]
    return mod26 (out - offsets[slot])

  bwd-pass slot/int idx/int -> int:
    contact := mod26 (idx + offsets[slot])
    out := BWD[rotors[slot]][contact]
    return mod26 (out - offsets[slot])

  press-key ch/int -> int:
    step
    c := pb[ch]
    c = fwd-pass 2 c
    c = fwd-pass 1 c
    c = fwd-pass 0 c
    c = REF[c]
    c = bwd-pass 0 c
    c = bwd-pass 1 c
    c = bwd-pass 2 c
    return pb[c]

  encrypt text/string -> string:
    result := ""
    text.to-ascii-upper.do: | c |
      if 'A' <= c <= 'Z':
        enc := press-key (c - 'A')
        result += string.from-rune ('A' + enc)
    return result

main:
  print "Enigma Cipher - Toit"
  tests := [
    [1,2,3,0,0,0,"AAAAA","BDZGO"],
    [1,2,3,0,0,0,"HELLOWORLD","ILBDAAMTAZ"],
    [1,2,3,0,0,0,"ATTACKATDAWN","BZHGNOCRRTCM"],
    [1,2,3,12,2,10,"HELLOWORLD","DLTBBQVPQV"],
    [3,1,2,0,0,0,"HELLOWORLD","KZHDFQYHXT"],
  ]
  tests.do: | t |
    e := Enigma.create t[0] t[1] t[2] t[3] t[4] t[5]
    result := e.encrypt t[6]
    ok := result == t[7] ? "[PASS]" : "[FAIL]"
    print "$(t[6]) -> $result $ok"
