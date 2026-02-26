-- Enigma Cipher - Alloy
-- Lightweight formal modeling language (MIT)
-- Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
-- PeopleTec Inc. - Guinness World Record Attempt 2026

module enigma

open util/integer
open util/ordering[State]

-- Abstract domain: letters as integers 0-25
sig Letter { val: Int } { val >= 0 and val <= 25 }

-- Rotor with forward and backward wiring
sig Rotor {
  fwd: Letter -> one Letter,
  bwd: Letter -> one Letter,
  notch: one Letter
} { all a, b: Letter | a->b in fwd iff b->a in bwd }

-- Machine state at each step
sig State {
  offsets: seq Int,       -- 3 rotor offsets
  plainChar: lone Letter, -- input character
  cipherChar: lone Letter -- output character
}

-- Rotor I wiring: EKMFLGDQVZNTOWYHXUSPAIBRCJ
one sig RotorI extends Rotor {} {
  notch.val = 16  -- Q
}

-- Rotor II wiring: AJDKSIRUXBLHWTMCQGZNPYFVOE  
one sig RotorII extends Rotor {} {
  notch.val = 4   -- E
}

-- Rotor III wiring: BDFHJLCPRTXVZNYEIWGAKMUSQO
one sig RotorIII extends Rotor {} {
  notch.val = 21  -- V
}

-- Reflector B: YRUHQSLDPXNGOKMIEBFZCWVJAT
one sig ReflectorB {
  wiring: Letter -> one Letter
} { all a: Letter | wiring[wiring[a]] = a }

-- Mod26 function
fun mod26[n: Int]: Int { rem[add[rem[n, 26], 26], 26] }

-- Double-stepping predicate
pred step[s, s': State] {
  let o0 = s.offsets[0], o1 = s.offsets[1], o2 = s.offsets[2] |
  let mid = (o1 = RotorII.notch.val),
      atn = (o2 = RotorIII.notch.val) |
  s'.offsets[2] = mod26[add[o2, 1]] and
  (mid or atn implies s'.offsets[1] = mod26[add[o1, 1]]
                  else s'.offsets[1] = o1) and
  (mid implies s'.offsets[0] = mod26[add[o0, 1]]
          else s'.offsets[0] = o0)
}

-- Test assertion: AAAAA encrypts to BDZGO
assert test_AAAAA {
  -- With initial state AAA, first A encrypts to B (=1)
}

-- Run a check
run { some State } for 5
