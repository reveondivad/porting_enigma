// Enigma Machine - Wren Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

var FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"]
var BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"]
var REF = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
var NOTCH = [16, 4, 21]

var mod26 = Fn.new {|n| ((n % 26) + 26) % 26 }

class Enigma {
    construct new(r1, r2, r3, k1, k2, k3, plugPairs) {
        _rotors = [r1 - 1, r2 - 1, r3 - 1]
        _offsets = [k1.bytes[0] - 65, k2.bytes[0] - 65, k3.bytes[0] - 65]
        _notches = [NOTCH[_rotors[0]], NOTCH[_rotors[1]], NOTCH[_rotors[2]]]
        _plugboard = List.filled(26, 0)
        for (i in 0...26) _plugboard[i] = i
        if (plugPairs != "") {
            for (pair in plugPairs.split("-")) {
                var a = pair.bytes[0] - 65
                var b = pair.bytes[1] - 65
                _plugboard[a] = b
                _plugboard[b] = a
            }
        }
    }

    step() {
        if (_offsets[1] == _notches[1]) {
            _offsets[1] = mod26.call(_offsets[1] + 1)
            _offsets[0] = mod26.call(_offsets[0] + 1)
        } else if (_offsets[2] == _notches[2]) {
            _offsets[1] = mod26.call(_offsets[1] + 1)
        }
        _offsets[2] = mod26.call(_offsets[2] + 1)
    }

    fwdPass(rotor, idx) {
        var contact = mod26.call(idx + _offsets[rotor])
        var out = FWD[_rotors[rotor]].bytes[contact] - 65
        return mod26.call(out - _offsets[rotor])
    }

    bwdPass(rotor, idx) {
        var contact = mod26.call(idx + _offsets[rotor])
        var out = BWD[_rotors[rotor]].bytes[contact] - 65
        return mod26.call(out - _offsets[rotor])
    }

    pressKey(c) {
        step()
        var idx = _plugboard[c.bytes[0] - 65]
        idx = fwdPass(2, idx)
        idx = fwdPass(1, idx)
        idx = fwdPass(0, idx)
        idx = REF.bytes[idx] - 65
        idx = bwdPass(0, idx)
        idx = bwdPass(1, idx)
        idx = bwdPass(2, idx)
        idx = _plugboard[idx]
        return String.fromCodePoint(65 + idx)
    }

    encrypt(text) {
        var result = ""
        for (c in text) {
            var code = c.bytes[0]
            if (code >= 97 && code <= 122) code = code - 32
            if (code >= 65 && code <= 90) {
                result = result + pressKey(String.fromCodePoint(code))
            }
        }
        return result
    }
}

System.print("Enigma Machine - Wren Implementation")
System.print("=====================================")

var tests = [
    [[1,2,3], "AAA", "", "AAAAA", "BDZGO"],
    [[1,2,3], "AAA", "", "HELLOWORLD", "ILBDAAMTAZ"],
    [[1,2,3], "AAA", "", "ATTACKATDAWN", "BZHGNOCRRTCM"],
    [[1,2,3], "MCK", "", "HELLOWORLD", "DLTBBQVPQV"],
    [[3,1,2], "AAA", "", "HELLOWORLD", "KZHDFQYHXT"],
    [[1,2,3], "AAA", "AB-CD-EF", "HELLOWORLD", "IKACBBMTBF"]
]

var pass = 0
for (i in 0...tests.count) {
    var t = tests[i]
    var r = t[0]
    var k = t[1]
    var e = Enigma.new(r[0], r[1], r[2], k[0], k[1], k[2], t[2])
    var result = e.encrypt(t[3])
    var ok = result == t[4]
    if (ok) pass = pass + 1
    var status = ok ? "[PASS]" : "[FAIL] expected %(t[4])"
    System.print("Test %(i+1): %(t[3]) -> %(result) %(status)")
}
System.print("\n%(pass)/6 tests passed")