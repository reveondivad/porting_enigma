# Enigma Machine - Nim Implementation
# Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
# PeopleTec Inc. - Guinness World Record Attempt 2026

import strutils, sequtils

const
  FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ",
         "AJDKSIRUXBLHWTMCQGZNPYFVOE",
         "BDFHJLCPRTXVZNYEIWGAKMUSQO"]
  BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ",
         "AJPCZWRLFBDKOTYUQGENHXMIVS",
         "TAGBPCSDQEUFVNZHYIXJWLRKOM"]
  NOTCH = [16, 4, 21]
  REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

proc mod26(a: int): int = ((a mod 26) + 26) mod 26

type
  Rotor = object
    fwd, bwd: string
    notch, offset: int

  Enigma = object
    left, middle, right: Rotor
    plug: array[26, int]

proc makeRotor(num: int, win: char): Rotor =
  Rotor(fwd: FWD[num], bwd: BWD[num], notch: NOTCH[num],
        offset: ord(win) - ord('A'))

proc forwardPass(r: Rotor, idx: int): int =
  let contact = mod26(idx + r.offset)
  mod26(ord(r.fwd[contact]) - ord('A') - r.offset)

proc backwardPass(r: Rotor, idx: int): int =
  let contact = mod26(idx + r.offset)
  mod26(ord(r.bwd[contact]) - ord('A') - r.offset)

proc step(r: var Rotor) = r.offset = (r.offset + 1) mod 26
proc atNotch(r: Rotor): bool = r.offset == r.notch

proc makeEnigma(rotors: array[3, int], key: string,
                plugboard: seq[string] = @[]): Enigma =
  result.left = makeRotor(rotors[0], key[0])
  result.middle = makeRotor(rotors[1], key[1])
  result.right = makeRotor(rotors[2], key[2])
  for i in 0..25: result.plug[i] = i
  for pair in plugboard:
    let a = ord(pair[0]) - ord('A')
    let b = ord(pair[1]) - ord('A')
    result.plug[a] = b; result.plug[b] = a

proc stepRotors(e: var Enigma) =
  if e.middle.atNotch:
    e.middle.step(); e.left.step()
  elif e.right.atNotch:
    e.middle.step()
  e.right.step()

proc pressKey(e: var Enigma, c: char): char =
  e.stepRotors()
  var idx = ord(c) - ord('A')
  idx = e.plug[idx]
  idx = e.right.forwardPass(idx)
  idx = e.middle.forwardPass(idx)
  idx = e.left.forwardPass(idx)
  idx = ord(REFL[idx]) - ord('A')
  idx = e.left.backwardPass(idx)
  idx = e.middle.backwardPass(idx)
  idx = e.right.backwardPass(idx)
  idx = e.plug[idx]
  chr(idx + ord('A'))

proc encrypt(e: var Enigma, text: string): string =
  result = ""
  for c in text.toUpperAscii:
    if c in 'A'..'Z':
      result.add(e.pressKey(c))

# Test harness
echo "Enigma Machine - Nim Implementation"
echo "===================================="

let tests = @[
  (@[0,1,2], "AAA", newSeq[string](), "AAAAA", "BDZGO"),
  (@[0,1,2], "AAA", newSeq[string](), "HELLOWORLD", "ILBDAAMTAZ"),
  (@[0,1,2], "AAA", newSeq[string](), "ATTACKATDAWN", "BZHGNOCRRTCM"),
  (@[0,1,2], "MCK", newSeq[string](), "HELLOWORLD", "DLTBBQVPQV"),
  (@[2,0,1], "AAA", newSeq[string](), "HELLOWORLD", "KZHDFQYHXT"),
  (@[0,1,2], "AAA", @["AB","CD","EF"], "HELLOWORLD", "IKACBBMTBF"),
]

var allPass = true
for i, t in tests:
  let (rotors, key, plugs, plain, expected) = t
  var e = makeEnigma([rotors[0], rotors[1], rotors[2]], key, plugs)
  let cipher = e.encrypt(plain)
  let ok = cipher == expected
  let status = if ok: "PASS" else: "FAIL"
  echo "  Test ", i+1, ": ", plain.alignLeft(20), " -> ", cipher.alignLeft(15), " [", status, "]"
  if not ok:
    echo "          Expected ", expected, ", got ", cipher
    allPass = false

echo(if allPass: "\n  ALL 6 TESTS PASSED" else: "\n  SOME TESTS FAILED")
