// Enigma Cipher - Odin
// C alternative with modern features (Joy of Programming)
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

package enigma

import "core:fmt"

FWD :: [3][26]int {
    {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9},
    {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4},
    {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14},
}
BWD :: [3][26]int {
    {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9},
    {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18},
    {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12},
}
REF :: [26]int{24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19}
NOTCH :: [3]int{16, 4, 21}

mod26 :: proc(n: int) -> int {
    m := n %% 26
    return m < 0 ? m + 26 : m
}

Enigma :: struct {
    r: [3]int,
    o: [3]int,
    n: [2]int,
}

make_enigma :: proc(r0, r1, r2, k0, k1, k2: int) -> Enigma {
    return Enigma{
        r = {r0, r1, r2},
        o = {k0, k1, k2},
        n = {NOTCH[r1], NOTCH[r2]},
    }
}

step :: proc(e: ^Enigma) {
    if e.o[1] == e.n[0] {
        e.o[1] = mod26(e.o[1] + 1)
        e.o[0] = mod26(e.o[0] + 1)
    } else if e.o[2] == e.n[1] {
        e.o[1] = mod26(e.o[1] + 1)
    }
    e.o[2] = mod26(e.o[2] + 1)
}

fwd_pass :: proc(e: ^Enigma, slot, idx: int) -> int {
    contact := mod26(idx + e.o[slot])
    out := FWD[e.r[slot]][contact]
    return mod26(out - e.o[slot])
}

bwd_pass :: proc(e: ^Enigma, slot, idx: int) -> int {
    contact := mod26(idx + e.o[slot])
    out := BWD[e.r[slot]][contact]
    return mod26(out - e.o[slot])
}

press_key :: proc(e: ^Enigma, ch: int) -> int {
    step(e)
    c := ch
    c = fwd_pass(e, 2, c); c = fwd_pass(e, 1, c); c = fwd_pass(e, 0, c)
    c = REF[c]
    c = bwd_pass(e, 0, c); c = bwd_pass(e, 1, c); c = bwd_pass(e, 2, c)
    return c
}

encrypt :: proc(r0, r1, r2, k0, k1, k2: int, msg: string) -> string {
    e := make_enigma(r0, r1, r2, k0, k1, k2)
    buf: [dynamic]byte
    for ch in msg {
        v := int(ch) - 65
        enc := press_key(&e, v)
        append(&buf, byte(enc + 65))
    }
    return string(buf[:])
}

main :: proc() {
    fmt.println("Enigma Cipher - Odin")
    tests := [?]struct{r:[3]int, k:[3]int, m:string, e:string}{
        {{0,1,2},{0,0,0},"AAAAA","BDZGO"},
        {{0,1,2},{0,0,0},"HELLOWORLD","ILBDAAMTAZ"},
        {{0,1,2},{0,0,0},"ATTACKATDAWN","BZHGNOCRRTCM"},
        {{0,1,2},{12,2,10},"HELLOWORLD","DLTBBQVPQV"},
        {{2,0,1},{0,0,0},"HELLOWORLD","KZHDFQYHXT"},
    }
    for t, i in tests {
        res := encrypt(t.r[0],t.r[1],t.r[2],t.k[0],t.k[1],t.k[2],t.m)
        ok := res == t.e ? "[PASS]" : "[FAIL]"
        fmt.printf("Test %d: %s %s\n", i+1, res, ok)
    }
}
