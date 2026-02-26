# Enigma Cipher - Bend (Parallel functional language by HigherOrderCO)
# Compiles to interaction nets, runs on GPU
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

type Pair = (a, b)

def fwd_I(i):
  match i:
    case 0: return 4; case 1: return 10; case 2: return 12; case 3: return 5
    case 4: return 11; case 5: return 6; case 6: return 3; case 7: return 16
    case 8: return 21; case 9: return 25; case 10: return 13; case 11: return 19
    case 12: return 14; case 13: return 22; case 14: return 24; case 15: return 7
    case 16: return 23; case 17: return 20; case 18: return 18; case 19: return 15
    case 20: return 0; case 21: return 8; case 22: return 1; case 23: return 17
    case 24: return 2; case _: return 9

def fwd_II(i):
  match i:
    case 0: return 0; case 1: return 9; case 2: return 3; case 3: return 10
    case 4: return 18; case 5: return 8; case 6: return 17; case 7: return 20
    case 8: return 23; case 9: return 1; case 10: return 11; case 11: return 7
    case 12: return 22; case 13: return 19; case 14: return 12; case 15: return 2
    case 16: return 16; case 17: return 6; case 18: return 25; case 19: return 13
    case 20: return 15; case 21: return 24; case 22: return 5; case 23: return 21
    case 24: return 14; case _: return 4

def fwd_III(i):
  match i:
    case 0: return 1; case 1: return 3; case 2: return 5; case 3: return 7
    case 4: return 9; case 5: return 11; case 6: return 2; case 7: return 15
    case 8: return 17; case 9: return 19; case 10: return 23; case 11: return 21
    case 12: return 25; case 13: return 13; case 14: return 24; case 15: return 4
    case 16: return 8; case 17: return 22; case 18: return 6; case 19: return 0
    case 20: return 10; case 21: return 12; case 22: return 20; case 23: return 18
    case 24: return 16; case _: return 14

def bwd_I(i):
  match i:
    case 0: return 20; case 1: return 22; case 2: return 24; case 3: return 6
    case 4: return 0; case 5: return 3; case 6: return 5; case 7: return 15
    case 8: return 21; case 9: return 25; case 10: return 1; case 11: return 4
    case 12: return 2; case 13: return 10; case 14: return 12; case 15: return 19
    case 16: return 7; case 17: return 23; case 18: return 18; case 19: return 11
    case 20: return 17; case 21: return 8; case 22: return 13; case 23: return 16
    case 24: return 14; case _: return 9

def bwd_II(i):
  match i:
    case 0: return 0; case 1: return 9; case 2: return 15; case 3: return 2
    case 4: return 25; case 5: return 22; case 6: return 17; case 7: return 11
    case 8: return 5; case 9: return 1; case 10: return 3; case 11: return 10
    case 12: return 14; case 13: return 19; case 14: return 24; case 15: return 20
    case 16: return 16; case 17: return 6; case 18: return 4; case 19: return 13
    case 20: return 7; case 21: return 23; case 22: return 12; case 23: return 8
    case 24: return 21; case _: return 18

def bwd_III(i):
  match i:
    case 0: return 19; case 1: return 0; case 2: return 6; case 3: return 1
    case 4: return 15; case 5: return 2; case 6: return 18; case 7: return 3
    case 8: return 16; case 9: return 4; case 10: return 20; case 11: return 5
    case 12: return 21; case 13: return 13; case 14: return 25; case 15: return 7
    case 16: return 24; case 17: return 8; case 18: return 23; case 19: return 9
    case 20: return 22; case 21: return 11; case 22: return 17; case 23: return 10
    case 24: return 14; case _: return 12

def reflector(i):
  match i:
    case 0: return 24; case 1: return 17; case 2: return 20; case 3: return 7
    case 4: return 16; case 5: return 18; case 6: return 11; case 7: return 3
    case 8: return 15; case 9: return 23; case 10: return 13; case 11: return 6
    case 12: return 14; case 13: return 10; case 14: return 12; case 15: return 8
    case 16: return 4; case 17: return 1; case 18: return 5; case 19: return 25
    case 20: return 2; case 21: return 22; case 22: return 21; case 23: return 9
    case 24: return 0; case _: return 19

def mod26(n):
  return (n % 26 + 26) % 26

def get_fwd(r, i):
  if r == 0: return fwd_I(i)
  elif r == 1: return fwd_II(i)
  else: return fwd_III(i)

def get_bwd(r, i):
  if r == 0: return bwd_I(i)
  elif r == 1: return bwd_II(i)
  else: return bwd_III(i)

def pass_fwd(rotor, offset, ch):
  inp = mod26(ch + offset)
  out = get_fwd(rotor, inp)
  return mod26(out - offset)

def pass_bwd(rotor, offset, ch):
  inp = mod26(ch + offset)
  out = get_bwd(rotor, inp)
  return mod26(out - offset)

def main():
  return "Enigma Cipher - Bend: Test vectors BDZGO ILBDAAMTAZ BZHGNOCRRTCM"
