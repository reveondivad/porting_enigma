// Enigma Cipher - Lobster
// Game programming language with flow-sensitive type checking
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

let fwd = [
    [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
    [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
    [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
]
let bwd = [
    [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
    [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
    [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
]
let ref = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
let notches = [16, 4, 21]

def mod26(n): ((n % 26) + 26) % 26

def pass_fwd(rotor, offset, ch):
    let inp = mod26(ch + offset)
    let out = fwd[rotor][inp]
    return mod26(out - offset)

def pass_bwd(rotor, offset, ch):
    let inp = mod26(ch + offset)
    let out = bwd[rotor][inp]
    return mod26(out - offset)

class EnigmaState:
    r0: int
    r1: int
    r2: int
    o0: int
    o1: int
    o2: int
    n1: int
    n2: int
    pb: [int]

def make_enigma(r0, r1, r2, k0, k1, k2) -> EnigmaState:
    let pb = map(26): _
    return EnigmaState { r0, r1, r2, o0: k0, o1: k1, o2: k2,
                         n1: notches[r1], n2: notches[r2], pb }

def step(s::EnigmaState):
    if s.o1 == s.n1:
        s.o1 = mod26(s.o1 + 1)
        s.o0 = mod26(s.o0 + 1)
    elif s.o2 == s.n2:
        s.o1 = mod26(s.o1 + 1)
    s.o2 = mod26(s.o2 + 1)

def press_key(s::EnigmaState, ch):
    step(s)
    var c = s.pb[ch]
    c = pass_fwd(s.r2, s.o2, c)
    c = pass_fwd(s.r1, s.o1, c)
    c = pass_fwd(s.r0, s.o0, c)
    c = ref[c]
    c = pass_bwd(s.r0, s.o0, c)
    c = pass_bwd(s.r1, s.o1, c)
    c = pass_bwd(s.r2, s.o2, c)
    return s.pb[c]

def encrypt(r0, r1, r2, k0, k1, k2, msg):
    let s = make_enigma(r0, r1, r2, k0, k1, k2)
    var result = ""
    for(msg) ch:
        let enc = press_key(s, ch - 65)
        result += string_from_int(enc + 65)  // char
    return result

print "Enigma Cipher - Lobster"
print "Test 1: " + encrypt(0,1,2,0,0,0,"AAAAA")
print "Test 2: " + encrypt(0,1,2,0,0,0,"HELLOWORLD")
print "Test 3: " + encrypt(0,1,2,0,0,0,"ATTACKATDAWN")
print "Test 4: " + encrypt(0,1,2,12,2,10,"HELLOWORLD")
print "Test 5: " + encrypt(2,0,1,0,0,0,"HELLOWORLD")
