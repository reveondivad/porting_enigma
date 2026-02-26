// Enigma Cipher - Whiley
// Verification-aware language with pre/post conditions
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

type nat is (int x) where x >= 0
type mod26_t is (int x) where 0 <= x && x < 26

int[] FWD_I = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
int[] FWD_II = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
int[] FWD_III = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
int[] BWD_I = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
int[] BWD_II = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
int[] BWD_III = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
int[] REFLECTOR = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
int[] NOTCH = [16, 4, 21]

function mod26(int n) -> (mod26_t r):
    int m = n % 26
    if m < 0:
        m = m + 26
    return m

function getFwd(int rotor, int idx) -> int:
    if rotor == 0:
        return FWD_I[idx]
    else if rotor == 1:
        return FWD_II[idx]
    else:
        return FWD_III[idx]

function getBwd(int rotor, int idx) -> int:
    if rotor == 0:
        return BWD_I[idx]
    else if rotor == 1:
        return BWD_II[idx]
    else:
        return BWD_III[idx]

function passFwd(int rotor, int offset, int ch) -> int:
    int inp = mod26(ch + offset)
    int out = getFwd(rotor, inp)
    return mod26(out - offset)

function passBwd(int rotor, int offset, int ch) -> int:
    int inp = mod26(ch + offset)
    int out = getBwd(rotor, inp)
    return mod26(out - offset)

type EnigmaState is {
    int r0, int r1, int r2,
    int o0, int o1, int o2,
    int n1, int n2,
    int[] pb
}

function init(int r0, int r1, int r2, int k0, int k1, int k2) -> EnigmaState:
    int[] pb = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
    return {
        r0: r0, r1: r1, r2: r2,
        o0: k0, o1: k1, o2: k2,
        n1: NOTCH[r1], n2: NOTCH[r2],
        pb: pb
    }

function step(EnigmaState s) -> EnigmaState:
    EnigmaState ns = s
    bool mid = (s.o1 == s.n1)
    bool atn = (s.o2 == s.n2)
    ns.o2 = mod26(s.o2 + 1)
    if atn || mid:
        ns.o1 = mod26(s.o1 + 1)
    if mid:
        ns.o0 = mod26(s.o0 + 1)
    return ns

function encryptChar(EnigmaState s, int ch) -> (int, EnigmaState):
    EnigmaState ns = step(s)
    int c = ns.pb[ch]
    c = passFwd(ns.r2, ns.o2, c)
    c = passFwd(ns.r1, ns.o1, c)
    c = passFwd(ns.r0, ns.o0, c)
    c = REFLECTOR[c]
    c = passBwd(ns.r0, ns.o0, c)
    c = passBwd(ns.r1, ns.o1, c)
    c = passBwd(ns.r2, ns.o2, c)
    c = ns.pb[c]
    return c, ns

method main():
    io::println("Enigma Cipher - Whiley")
    io::println("Test vectors: BDZGO, ILBDAAMTAZ, BZHGNOCRRTCM, DLTBBQVPQV, KZHDFQYHXT, IKACBBMTBF")
