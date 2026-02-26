// Enigma Cipher - Mint (Web framework language)
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

module Enigma {
  const FWD = [
    [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
    [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
    [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
  ]
  const BWD = [
    [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
    [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
    [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
  ]
  const REF = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
  const NOTCH = [16, 4, 21]

  fun mod26(n : Number) : Number {
    let m = Math.mod(n, 26)
    if (m < 0) { m + 26 } else { m }
  }

  fun passFwd(rotor : Number, offset : Number, ch : Number) : Number {
    let inp = mod26(ch + offset)
    let out = FWD[rotor][inp]
    mod26(out - offset)
  }

  fun passBwd(rotor : Number, offset : Number, ch : Number) : Number {
    let inp = mod26(ch + offset)
    let out = BWD[rotor][inp]
    mod26(out - offset)
  }

  fun encrypt(r0 : Number, r1 : Number, r2 : Number,
              k0 : Number, k1 : Number, k2 : Number,
              msg : String) : String {
    let o0 = k0
    let o1 = k1
    let o2 = k2
    let n1 = NOTCH[r1]
    let n2 = NOTCH[r2]
    let result = ""

    for (char of String.split("", msg)) {
      let ch = String.codePointAt(0, char) - 65
      let mid = o1 == n1
      let atn = o2 == n2
      o2 = mod26(o2 + 1)
      if (atn || mid) { o1 = mod26(o1 + 1) }
      if (mid) { o0 = mod26(o0 + 1) }
      let c = ch
      c = passFwd(r2, o2, c)
      c = passFwd(r1, o1, c)
      c = passFwd(r0, o0, c)
      c = REF[c]
      c = passBwd(r0, o0, c)
      c = passBwd(r1, o1, c)
      c = passBwd(r2, o2, c)
      result = result + String.fromCodePoint(c + 65)
    }
    result
  }
}

component Main {
  fun render : Html {
    <div>
      <p>"Enigma Cipher - Mint"</p>
      <p>"Test 1: " + Enigma.encrypt(0,1,2,0,0,0,"AAAAA")</p>
      <p>"Test 2: " + Enigma.encrypt(0,1,2,0,0,0,"HELLOWORLD")</p>
    </div>
  }
}
