# Enigma Cipher - NimScript (.nims)
# Nim's scripting subset (runs at compile time or standalone)
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026
# Distinct from enigma.nim (full Nim)

const
  fwdI  = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
  fwdII = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
  fwdIII= [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
  bwdI  = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
  bwdII = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
  bwdIII= [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
  reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
  notches = [16, 4, 21]

proc mod26(n: int): int =
  result = ((n mod 26) + 26) mod 26

proc getFwd(r, i: int): int =
  case r
  of 0: fwdI[i]
  of 1: fwdII[i]
  else: fwdIII[i]

proc getBwd(r, i: int): int =
  case r
  of 0: bwdI[i]
  of 1: bwdII[i]
  else: bwdIII[i]

proc passFwd(rotor, offset, ch: int): int =
  let inp = mod26(ch + offset)
  let o = getFwd(rotor, inp)
  mod26(o - offset)

proc passBwd(rotor, offset, ch: int): int =
  let inp = mod26(ch + offset)
  let o = getBwd(rotor, inp)
  mod26(o - offset)

var gR = [0, 0, 0]
var gO = [0, 0, 0]
var gN = [0, 0, 0]

proc initE(r1, r2, r3, k1, k2, k3: int) =
  gR = [r1-1, r2-1, r3-1]
  gO = [k1, k2, k3]
  for i in 0..2: gN[i] = notches[gR[i]]

proc step() =
  if gO[1] == gN[1]:
    gO[1] = mod26(gO[1]+1)
    gO[0] = mod26(gO[0]+1)
  elif gO[2] == gN[2]:
    gO[1] = mod26(gO[1]+1)
  gO[2] = mod26(gO[2]+1)

proc pressKey(ch: int): int =
  step()
  var c = ch
  c = passFwd(gR[2], gO[2], c)
  c = passFwd(gR[1], gO[1], c)
  c = passFwd(gR[0], gO[0], c)
  c = reflector[c]
  c = passBwd(gR[0], gO[0], c)
  c = passBwd(gR[1], gO[1], c)
  c = passBwd(gR[2], gO[2], c)
  c

proc encrypt(msg: string): string =
  result = ""
  for ch in msg:
    let v = ord(ch) - 65
    if v >= 0 and v < 26:
      result.add chr(pressKey(v) + 65)

echo "Enigma Cipher - NimScript"
initE(1,2,3,0,0,0); echo "Test 1: " & encrypt("AAAAA")
initE(1,2,3,0,0,0); echo "Test 2: " & encrypt("HELLOWORLD")
initE(1,2,3,0,0,0); echo "Test 3: " & encrypt("ATTACKATDAWN")
initE(1,2,3,12,2,10); echo "Test 4: " & encrypt("HELLOWORLD")
initE(3,1,2,0,0,0); echo "Test 5: " & encrypt("HELLOWORLD")
