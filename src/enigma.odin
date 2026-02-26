// Enigma Cipher - Odin
// Systems programming language, alternative to C

package main

import "core:fmt"
import "core:strings"

FWD :: [3][26]int{
    {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9},
    {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4},
    {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14},
}

BWD :: [3][26]int{
    {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9},
    {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18},
    {19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10},
}

NOTCH :: [3]int{16, 4, 21}
REFLECTOR :: [26]int{24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19}

Enigma :: struct {
    rotors: [3]int,
    offsets: [3]int,
    plugboard: [26]int,
}

mod26 :: proc(x: int) -> int {
    return ((x % 26) + 26) % 26
}

make_plugboard :: proc(pairs: [][2]int) -> [26]int {
    pb: [26]int
    for i in 0..<26 { pb[i] = i }
    for p in pairs {
        pb[p[0]] = p[1]
        pb[p[1]] = p[0]
    }
    return pb
}

step :: proc(e: ^Enigma) {
    r0, r1, r2 := e.rotors[0], e.rotors[1], e.rotors[2]
    mid := e.offsets[1] == NOTCH[r1]
    e.offsets[2] = mod26(e.offsets[2] + 1)
    if e.offsets[2] == mod26(NOTCH[r2] + 1) || mid {
        e.offsets[1] = mod26(e.offsets[1] + 1)
    }
    if mid {
        e.offsets[0] = mod26(e.offsets[0] + 1)
    }
}

// Fix: step before encrypt uses pre-step notch check
encrypt_char :: proc(e: ^Enigma, ch: int) -> int {
    // Step rotors
    r0, r1, r2 := e.rotors[0], e.rotors[1], e.rotors[2]
    mid := e.offsets[1] == NOTCH[r1]
    at_notch2 := e.offsets[2] == NOTCH[r2]
    e.offsets[2] = mod26(e.offsets[2] + 1)
    if at_notch2 || mid {
        e.offsets[1] = mod26(e.offsets[1] + 1)
    }
    if mid {
        e.offsets[0] = mod26(e.offsets[0] + 1)
    }

    o0, o1, o2 := e.offsets[0], e.offsets[1], e.offsets[2]
    c := e.plugboard[ch]
    // Forward through rotors R-M-L
    c = mod26(FWD[r2][mod26(c + o2)] - o2)
    c = mod26(FWD[r1][mod26(c + o1)] - o1)
    c = mod26(FWD[r0][mod26(c + o0)] - o0)
    // Reflector
    c = REFLECTOR[c]
    // Backward through rotors L-M-R
    c = mod26(BWD[r0][mod26(c + o0)] - o0)
    c = mod26(BWD[r1][mod26(c + o1)] - o1)
    c = mod26(BWD[r2][mod26(c + o2)] - o2)
    // Plugboard out
    c = e.plugboard[c]
    return c
}

encrypt :: proc(rotors: [3]int, key: [3]int, pairs: [][2]int, msg: string) -> string {
    e := Enigma{
        rotors = rotors,
        offsets = key,
        plugboard = make_plugboard(pairs),
    }
    buf: [dynamic]u8
    for ch in msg {
        c := encrypt_char(&e, int(ch) - 65)
        append(&buf, u8(c + 65))
    }
    return string(buf[:])
}

run_test :: proc(label, expected, actual: string) {
    status := "PASS" if expected == actual else "FAIL"
    fmt.printf("%s %s: %s (expected %s)\n", status, label, actual, expected)
}

main :: proc() {
    fmt.println("Enigma Cipher - Odin")
    no_pairs: [][2]int
    run_test("Test 1", "BDZGO",
        encrypt({0,1,2}, {0,0,0}, no_pairs, "AAAAA"))
    run_test("Test 2", "ILBDAAMTAZ",
        encrypt({0,1,2}, {0,0,0}, no_pairs, "HELLOWORLD"))
    run_test("Test 3", "BZHGNOCRRTCM",
        encrypt({0,1,2}, {0,0,0}, no_pairs, "ATTACKATDAWN"))
    run_test("Test 4", "DLTBBQVPQV",
        encrypt({0,1,2}, {12,2,10}, no_pairs, "HELLOWORLD"))
    run_test("Test 5", "KZHDFQYHXT",
        encrypt({2,0,1}, {0,0,0}, no_pairs, "HELLOWORLD"))
    pairs := [][2]int{{0,1}, {2,3}, {4,5}}
    run_test("Test 6", "IKACBBMTBF",
        encrypt({0,1,2}, {0,0,0}, pairs, "HELLOWORLD"))
}
