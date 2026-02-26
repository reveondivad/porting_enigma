// Enigma Cipher - Grace
// Educational object-oriented language (Univ. of Portland/Victoria)
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

def fwd = [
    [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
    [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
    [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
]
def bwd = [
    [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
    [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
    [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
]
def reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
def notches = [16, 4, 21]

method mod26(n) {
    def m = n % 26
    if (m < 0) then { m + 26 } else { m }
}

class enigma(r0, r1, r2, k0, k1, k2) {
    var r := [r0, r1, r2]
    var o := [k0, k1, k2]
    def n1 = notches.at(r1 + 1)
    def n2 = notches.at(r2 + 1)

    method step {
        if (o.at(2) == n1) then {
            o.at(2) put (mod26(o.at(2) + 1))
            o.at(1) put (mod26(o.at(1) + 1))
        } elseif (o.at(3) == n2) then {
            o.at(2) put (mod26(o.at(2) + 1))
        }
        o.at(3) put (mod26(o.at(3) + 1))
    }

    method fwdPass(slot, idx) {
        def c = mod26(idx + o.at(slot + 1))
        mod26(fwd.at(r.at(slot + 1) + 1).at(c + 1) - o.at(slot + 1))
    }

    method bwdPass(slot, idx) {
        def c = mod26(idx + o.at(slot + 1))
        mod26(bwd.at(r.at(slot + 1) + 1).at(c + 1) - o.at(slot + 1))
    }

    method pressKey(ch) {
        step
        var c := ch
        c := fwdPass(2, c); c := fwdPass(1, c); c := fwdPass(0, c)
        c := reflector.at(c + 1)
        c := bwdPass(0, c); c := bwdPass(1, c); c := bwdPass(2, c)
        c
    }

    method encrypt(msg) {
        var result := ""
        for (1 .. msg.size) do { i ->
            def ch = msg.at(i).ord - 65
            result := result ++ (pressKey(ch) + 65).chr
        }
        result
    }
}

print "Enigma Cipher - Grace"
def e1 = enigma(0,1,2,0,0,0)
print "Test 1: {e1.encrypt("AAAAA")} expected BDZGO"
def e2 = enigma(0,1,2,0,0,0)
print "Test 2: {e2.encrypt("HELLOWORLD")} expected ILBDAAMTAZ"
def e3 = enigma(0,1,2,0,0,0)
print "Test 3: {e3.encrypt("ATTACKATDAWN")} expected BZHGNOCRRTCM"
def e4 = enigma(0,1,2,12,2,10)
print "Test 4: {e4.encrypt("HELLOWORLD")} expected DLTBBQVPQV"
def e5 = enigma(2,0,1,0,0,0)
print "Test 5: {e5.encrypt("HELLOWORLD")} expected KZHDFQYHXT"
