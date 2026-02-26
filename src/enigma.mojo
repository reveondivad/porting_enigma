# Enigma Cipher - Mojo 🔥
# High-performance Python superset by Modular

from collections import List

alias FWD_I = List[Int](4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9)
alias FWD_II = List[Int](0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4)
alias FWD_III = List[Int](1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14)

alias BWD_I = List[Int](20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9)
alias BWD_II = List[Int](0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18)
alias BWD_III = List[Int](19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12)

alias NOTCHES = List[Int](16, 4, 21)
alias REFLECTOR = List[Int](24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19)

fn mod26(x: Int) -> Int:
    var m = x % 26
    if m < 0:
        m += 26
    return m

fn get_fwd(r: Int, i: Int) -> Int:
    if r == 0: return FWD_I[i]
    elif r == 1: return FWD_II[i]
    else: return FWD_III[i]

fn get_bwd(r: Int, i: Int) -> Int:
    if r == 0: return BWD_I[i]
    elif r == 1: return BWD_II[i]
    else: return BWD_III[i]

fn pass_fwd(rotor: Int, offset: Int, ch: Int) -> Int:
    var inp = mod26(ch + offset)
    var out = get_fwd(rotor, inp)
    return mod26(out - offset)

fn pass_bwd(rotor: Int, offset: Int, ch: Int) -> Int:
    var inp = mod26(ch + offset)
    var out = get_bwd(rotor, inp)
    return mod26(out - offset)

struct Enigma:
    var r0: Int
    var r1: Int
    var r2: Int
    var o0: Int
    var o1: Int
    var o2: Int
    var pb: List[Int]

    fn __init__(inout self, r0: Int, r1: Int, r2: Int,
                k0: Int, k1: Int, k2: Int,
                pairs: List[Tuple[Int,Int]]):
        self.r0 = r0; self.r1 = r1; self.r2 = r2
        self.o0 = k0; self.o1 = k1; self.o2 = k2
        self.pb = List[Int]()
        for i in range(26):
            self.pb.append(i)
        for p in pairs:
            self.pb[p[0]] = p[1]
            self.pb[p[1]] = p[0]

    fn encrypt_char(inout self, ch: Int) -> Int:
        # Step rotors
        var mid = self.o1 == NOTCHES[self.r1]
        var atn = self.o2 == NOTCHES[self.r2]
        self.o2 = mod26(self.o2 + 1)
        if atn or mid:
            self.o1 = mod26(self.o1 + 1)
        if mid:
            self.o0 = mod26(self.o0 + 1)
        # Encrypt
        var c = self.pb[ch]
        c = pass_fwd(self.r2, self.o2, c)
        c = pass_fwd(self.r1, self.o1, c)
        c = pass_fwd(self.r0, self.o0, c)
        c = REFLECTOR[c]
        c = pass_bwd(self.r0, self.o0, c)
        c = pass_bwd(self.r1, self.o1, c)
        c = pass_bwd(self.r2, self.o2, c)
        c = self.pb[c]
        return c

    fn encrypt(inout self, msg: String) -> String:
        var result = String()
        for i in range(len(msg)):
            var ch = ord(msg[i]) - ord('A')
            var c = self.encrypt_char(ch)
            result += chr(c + ord('A'))
        return result

fn run_test(label: String, expected: String,
            rotors: Tuple[Int,Int,Int], key: Tuple[Int,Int,Int],
            pairs: List[Tuple[Int,Int]], msg: String):
    var e = Enigma(rotors[0], rotors[1], rotors[2],
                   key[0], key[1], key[2], pairs)
    var actual = e.encrypt(msg)
    var status = "PASS" if expected == actual else "FAIL"
    print(status, label + ": " + actual + " (expected " + expected + ")")

fn main():
    print("Enigma Cipher - Mojo")
    var no_pairs = List[Tuple[Int,Int]]()
    var pairs = List[Tuple[Int,Int]]((0,1), (2,3), (4,5))

    run_test("Test 1", "BDZGO", (0,1,2), (0,0,0), no_pairs, "AAAAA")
    run_test("Test 2", "ILBDAAMTAZ", (0,1,2), (0,0,0), no_pairs, "HELLOWORLD")
    run_test("Test 3", "BZHGNOCRRTCM", (0,1,2), (0,0,0), no_pairs, "ATTACKATDAWN")
    run_test("Test 4", "DLTBBQVPQV", (0,1,2), (12,2,10), no_pairs, "HELLOWORLD")
    run_test("Test 5", "KZHDFQYHXT", (2,0,1), (0,0,0), no_pairs, "HELLOWORLD")
    run_test("Test 6", "IKACBBMTBF", (0,1,2), (0,0,0), pairs, "HELLOWORLD")
