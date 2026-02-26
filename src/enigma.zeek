# Enigma Cipher - Zeek (formerly Bro) Network Security Monitor scripting
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

global fwdI:  vector of count = vector(4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9);
global fwdII: vector of count = vector(0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4);
global fwdIII:vector of count = vector(1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14);
global bwdI:  vector of count = vector(20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9);
global bwdII: vector of count = vector(0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18);
global bwdIII:vector of count = vector(19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12);
global ref:   vector of count = vector(24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19);
global notches: vector of count = vector(16, 4, 21);

function mod26(n: int): count {
    local m = n % 26;
    if (m < 0) m += 26;
    return m;
}

function get_fwd(r: count, i: count): count {
    if (r == 0) return fwdI[i];
    if (r == 1) return fwdII[i];
    return fwdIII[i];
}

function get_bwd(r: count, i: count): count {
    if (r == 0) return bwdI[i];
    if (r == 1) return bwdII[i];
    return bwdIII[i];
}

function pass_fwd(rotor: count, offset: count, ch: count): count {
    local inp = mod26(ch + offset);
    local out = get_fwd(rotor, inp);
    return mod26(out - offset);
}

function pass_bwd(rotor: count, offset: count, ch: count): count {
    local inp = mod26(ch + offset);
    local out = get_bwd(rotor, inp);
    return mod26(out - offset);
}

type EnigmaState: record {
    r0: count; r1: count; r2: count;
    o0: count; o1: count; o2: count;
    n1: count; n2: count;
};

function enigma_encrypt(r0: count, r1: count, r2: count,
                        k0: count, k1: count, k2: count,
                        msg: string): string {
    local o0 = k0; local o1 = k1; local o2 = k2;
    local n1 = notches[r1]; local n2 = notches[r2];
    local result = "";

    for (i in |msg|) {
        local ch = bytestring_to_count(msg[i]) - 65;
        local mid = (o1 == n1);
        local atn = (o2 == n2);
        o2 = mod26(o2 + 1);
        if (atn || mid) o1 = mod26(o1 + 1);
        if (mid) o0 = mod26(o0 + 1);
        local c = ch;
        c = pass_fwd(r2, o2, c); c = pass_fwd(r1, o1, c); c = pass_fwd(r0, o0, c);
        c = ref[c];
        c = pass_bwd(r0, o0, c); c = pass_bwd(r1, o1, c); c = pass_bwd(r2, o2, c);
        result += fmt("%c", c + 65);
    }
    return result;
}

event zeek_init() {
    print fmt("Enigma Cipher - Zeek");
    print fmt("Test 1: %s expected BDZGO", enigma_encrypt(0,1,2,0,0,0,"AAAAA"));
    print fmt("Test 2: %s expected ILBDAAMTAZ", enigma_encrypt(0,1,2,0,0,0,"HELLOWORLD"));
    print fmt("Test 3: %s expected BZHGNOCRRTCM", enigma_encrypt(0,1,2,0,0,0,"ATTACKATDAWN"));
}
