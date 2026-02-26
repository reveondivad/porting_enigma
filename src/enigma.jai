// Enigma Cipher - Jai (Jonathan Blow's language)
// Game programming language with compile-time execution
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

#import "Basic";

FWD :: int.[3][26]{
    .[4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
    .[0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
    .[1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14],
};
BWD :: int.[3][26]{
    .[20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
    .[0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
    .[19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12],
};
REF :: int.[26]{24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
NOTCH :: int.[3]{16, 4, 21};

mod26 :: (n: int) -> int {
    m := n % 26;
    if m < 0 return m + 26;
    return m;
}

Enigma_State :: struct {
    rotors:  [3]int;
    offsets: [3]int;
    notches: [3]int;
    pb:      [26]int;
}

init_enigma :: (r1: int, r2: int, r3: int, k1: int, k2: int, k3: int) -> Enigma_State {
    s: Enigma_State;
    s.rotors  = .[r1-1, r2-1, r3-1];
    s.offsets = .[k1, k2, k3];
    for i: 0..2  s.notches[i] = NOTCH[s.rotors[i]];
    for i: 0..25 s.pb[i] = i;
    return s;
}

step :: (s: *Enigma_State) {
    if s.offsets[1] == s.notches[1] {
        s.offsets[1] = mod26(s.offsets[1] + 1);
        s.offsets[0] = mod26(s.offsets[0] + 1);
    } else if s.offsets[2] == s.notches[2] {
        s.offsets[1] = mod26(s.offsets[1] + 1);
    }
    s.offsets[2] = mod26(s.offsets[2] + 1);
}

fwd_pass :: (s: *Enigma_State, slot: int, idx: int) -> int {
    contact := mod26(idx + s.offsets[slot]);
    out := FWD[s.rotors[slot]][contact];
    return mod26(out - s.offsets[slot]);
}

bwd_pass :: (s: *Enigma_State, slot: int, idx: int) -> int {
    contact := mod26(idx + s.offsets[slot]);
    out := BWD[s.rotors[slot]][contact];
    return mod26(out - s.offsets[slot]);
}

press_key :: (s: *Enigma_State, ch: int) -> int {
    step(s);
    c := s.pb[ch];
    c = fwd_pass(s, 2, c); c = fwd_pass(s, 1, c); c = fwd_pass(s, 0, c);
    c = REF[c];
    c = bwd_pass(s, 0, c); c = bwd_pass(s, 1, c); c = bwd_pass(s, 2, c);
    return s.pb[c];
}

encrypt :: (r1: int, r2: int, r3: int, k1: int, k2: int, k3: int, msg: string) -> string {
    s := init_enigma(r1, r2, r3, k1, k2, k3);
    builder: String_Builder;
    for ch: msg {
        if ch >= #char "A" && ch <= #char "Z" {
            enc := press_key(*s, ch - #char "A");
            append(*builder, cast(u8)(enc + 65));
        }
    }
    return builder_to_string(*builder);
}

main :: () {
    print("Enigma Cipher - Jai\n");
    print("Test 1: %\n", encrypt(1,2,3,0,0,0,"AAAAA"));
    print("Test 2: %\n", encrypt(1,2,3,0,0,0,"HELLOWORLD"));
    print("Test 3: %\n", encrypt(1,2,3,0,0,0,"ATTACKATDAWN"));
    print("Test 4: %\n", encrypt(1,2,3,12,2,10,"HELLOWORLD"));
    print("Test 5: %\n", encrypt(3,1,2,0,0,0,"HELLOWORLD"));
}
