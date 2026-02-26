// Enigma Machine - Fantom Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

class Enigma {
  static const Str[] fwd := ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"]
  static const Str[] bwd := ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"]
  static const Str ref := "YRUHQSLDPXNGOKMIEBFZCWVJAT"
  static const Int[] notch := [16, 4, 21]

  Int[] rotors; Int[] offsets; Int[] notches; Int[] plugboard

  new make(Int r1, Int r2, Int r3, Int k1, Int k2, Int k3, Str plugPairs := "") {
    rotors = [r1-1, r2-1, r3-1]
    offsets = [k1, k2, k3]
    notches = [notch[rotors[0]], notch[rotors[1]], notch[rotors[2]]]
    plugboard = (0..<26).toList
    if (!plugPairs.isEmpty) {
      plugPairs.split('-').each |pair| {
        a := pair[0] - 'A'; b := pair[1] - 'A'
        plugboard[a] = b; plugboard[b] = a
      }
    }
  }

  static Int mod26(Int n) { ((n % 26) + 26) % 26 }

  Void step() {
    if (offsets[1] == notches[1]) {
      offsets[1] = mod26(offsets[1] + 1)
      offsets[0] = mod26(offsets[0] + 1)
    } else if (offsets[2] == notches[2]) {
      offsets[1] = mod26(offsets[1] + 1)
    }
    offsets[2] = mod26(offsets[2] + 1)
  }

  Int fwdPass(Int rotor, Int idx) {
    contact := mod26(idx + offsets[rotor])
    out := fwd[rotors[rotor]][contact] - 'A'
    return mod26(out - offsets[rotor])
  }

  Int bwdPass(Int rotor, Int idx) {
    contact := mod26(idx + offsets[rotor])
    out := bwd[rotors[rotor]][contact] - 'A'
    return mod26(out - offsets[rotor])
  }

  Int pressKey(Int c) {
    step
    idx := plugboard[c - 'A']
    idx = fwdPass(2, idx); idx = fwdPass(1, idx); idx = fwdPass(0, idx)
    idx = ref[idx] - 'A'
    idx = bwdPass(0, idx); idx = bwdPass(1, idx); idx = bwdPass(2, idx)
    idx = plugboard[idx]
    return 'A' + idx
  }

  Str encrypt(Str text) {
    buf := StrBuf()
    text.upper.each |c| { if (c >= 'A' && c <= 'Z') buf.addChar(pressKey(c)) }
    return buf.toStr
  }

  static Void main(Str[] args) {
    echo("Enigma Machine - Fantom Implementation")
    tests := [
      [[1,2,3], "AAA", "", "AAAAA", "BDZGO"],
      [[1,2,3], "AAA", "", "HELLOWORLD", "ILBDAAMTAZ"],
      [[1,2,3], "AAA", "", "ATTACKATDAWN", "BZHGNOCRRTCM"],
      [[1,2,3], "MCK", "", "HELLOWORLD", "DLTBBQVPQV"],
      [[3,1,2], "AAA", "", "HELLOWORLD", "KZHDFQYHXT"],
      [[1,2,3], "AAA", "AB-CD-EF", "HELLOWORLD", "IKACBBMTBF"],
    ]
    pass := 0
    tests.each |t, i| {
      r := t[0] as Int[]; k := t[1] as Str
      e := Enigma(r[0], r[1], r[2], k[0]-'A', k[1]-'A', k[2]-'A', t[2])
      result := e.encrypt(t[3])
      ok := result == t[4]
      if (ok) pass++
      echo("Test ${i+1}: ${t[3]} -> $result ${ok ? "[PASS]" : "[FAIL] expected ${t[4]}"}")
    }
    echo("$pass/6 tests passed")
  }
}
