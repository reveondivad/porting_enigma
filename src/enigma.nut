// Enigma Machine - Squirrel Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

local FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"];
local BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"];
local NOTCH = [16, 4, 21];
local REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

function mod26(a) { return ((a % 26) + 26) % 26; }
function c2i(c) { return c - 'A'; }
function i2c(i) { return (i + 'A').tochar(); }

class Rotor {
    fwd = null; bwd = null; notch = 0; offset = 0;
    constructor(num, win) {
        fwd = FWD[num]; bwd = BWD[num];
        notch = NOTCH[num]; offset = c2i(win);
    }
    function forward(idx) {
        local contact = mod26(idx + offset);
        return mod26(c2i(fwd[contact]) - offset);
    }
    function backward(idx) {
        local contact = mod26(idx + offset);
        return mod26(c2i(bwd[contact]) - offset);
    }
    function step() { offset = (offset + 1) % 26; }
    function atNotch() { return offset == notch; }
}

class Enigma {
    left = null; middle = null; right = null; plug = null;
    constructor(rotors, key, plugboard) {
        left = Rotor(rotors[0], key[0]);
        middle = Rotor(rotors[1], key[1]);
        right = Rotor(rotors[2], key[2]);
        plug = array(26);
        for (local i = 0; i < 26; i++) plug[i] = i;
        if (plugboard != null) {
            foreach (idx, pair in plugboard) {
                local a = c2i(pair[0]), b = c2i(pair[1]);
                plug[a] = b; plug[b] = a;
            }
        }
    }
    function stepRotors() {
        if (middle.atNotch()) { middle.step(); left.step(); }
        else if (right.atNotch()) { middle.step(); }
        right.step();
    }
    function pressKey(c) {
        stepRotors();
        local idx = c2i(c);
        idx = plug[idx];
        idx = right.forward(idx);
        idx = middle.forward(idx);
        idx = left.forward(idx);
        idx = c2i(REFL[idx]);
        idx = left.backward(idx);
        idx = middle.backward(idx);
        idx = right.backward(idx);
        idx = plug[idx];
        return i2c(idx);
    }
    function encrypt(text) {
        local result = "";
        local upper = text.toupper();
        for (local i = 0; i < upper.len(); i++) {
            local c = upper[i];
            if (c >= 'A' && c <= 'Z') result += pressKey(c);
        }
        return result;
    }
}

print("Enigma Machine - Squirrel Implementation\n");
print("=========================================\n");

local tests = [
    [[0,1,2], "AAA", null,             "AAAAA",        "BDZGO"],
    [[0,1,2], "AAA", null,             "HELLOWORLD",   "ILBDAAMTAZ"],
    [[0,1,2], "AAA", null,             "ATTACKATDAWN", "BZHGNOCRRTCM"],
    [[0,1,2], "MCK", null,             "HELLOWORLD",   "DLTBBQVPQV"],
    [[2,0,1], "AAA", null,             "HELLOWORLD",   "KZHDFQYHXT"],
    [[0,1,2], "AAA", ["AB","CD","EF"], "HELLOWORLD",   "IKACBBMTBF"]
];

local allPass = true;
foreach (i, t in tests) {
    local e = Enigma(t[0], t[1], t[2]);
    local cipher = e.encrypt(t[3]);
    local ok = cipher == t[4];
    local status = ok ? "PASS" : "FAIL";
    print(format("  Test %d: %s -> %s [%s]\n", i+1, t[3], cipher, status));
    if (!ok) { print(format("          Expected %s\n", t[4])); allPass = false; }
}
print(allPass ? "\n  ALL 6 TESTS PASSED\n" : "\n  SOME TESTS FAILED\n");
