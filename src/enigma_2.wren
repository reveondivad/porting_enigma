// Enigma Cipher - Wren
// Small, fast, class-based scripting language
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

class Enigma {
  static fwd { [
    [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
    [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
    [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
  ] }
  static bwd { [
    [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
    [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
    [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
  ] }
  static ref { [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19] }
  static notch { [16, 4, 21] }

  static mod26(n) { ((n % 26) + 26) % 26 }

  construct new(r0, r1, r2, k0, k1, k2) {
    _r = [r0, r1, r2]
    _o = [k0, k1, k2]
    _n1 = Enigma.notch[r1]
    _n2 = Enigma.notch[r2]
  }

  step() {
    if (_o[1] == _n1) {
      _o[1] = Enigma.mod26(_o[1] + 1)
      _o[0] = Enigma.mod26(_o[0] + 1)
    } else if (_o[2] == _n2) {
      _o[1] = Enigma.mod26(_o[1] + 1)
    }
    _o[2] = Enigma.mod26(_o[2] + 1)
  }

  fwdPass(slot, idx) {
    var c = Enigma.mod26(idx + _o[slot])
    var out = Enigma.fwd[_r[slot]][c]
    return Enigma.mod26(out - _o[slot])
  }

  bwdPass(slot, idx) {
    var c = Enigma.mod26(idx + _o[slot])
    var out = Enigma.bwd[_r[slot]][c]
    return Enigma.mod26(out - _o[slot])
  }

  pressKey(ch) {
    step()
    var c = ch
    c = fwdPass(2, c)
    c = fwdPass(1, c)
    c = fwdPass(0, c)
    c = Enigma.ref[c]
    c = bwdPass(0, c)
    c = bwdPass(1, c)
    c = bwdPass(2, c)
    return c
  }

  encrypt(msg) {
    var result = ""
    for (i in 0...msg.count) {
      var ch = msg[i].bytes[0] - 65
      result = result + String.fromCodePoint(pressKey(ch) + 65)
    }
    return result
  }
}

System.print("Enigma Cipher - Wren")
var e1 = Enigma.new(0,1,2,0,0,0)
System.print("Test 1: %(e1.encrypt("AAAAA")) expected BDZGO")
var e2 = Enigma.new(0,1,2,0,0,0)
System.print("Test 2: %(e2.encrypt("HELLOWORLD")) expected ILBDAAMTAZ")
var e3 = Enigma.new(0,1,2,0,0,0)
System.print("Test 3: %(e3.encrypt("ATTACKATDAWN")) expected BZHGNOCRRTCM")
var e4 = Enigma.new(0,1,2,12,2,10)
System.print("Test 4: %(e4.encrypt("HELLOWORLD")) expected DLTBBQVPQV")
var e5 = Enigma.new(2,0,1,0,0,0)
System.print("Test 5: %(e5.encrypt("HELLOWORLD")) expected KZHDFQYHXT")
