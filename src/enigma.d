// Enigma Machine - D Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

import std.stdio, std.string, std.algorithm, std.conv, std.uni;

immutable string[3] FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ",
                            "AJDKSIRUXBLHWTMCQGZNPYFVOE",
                            "BDFHJLCPRTXVZNYEIWGAKMUSQO"];
immutable string[3] BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ",
                            "AJPCZWRLFBDKOTYUQGENHXMIVS",
                            "TAGBPCSDQEUFVNZHYIXJWLRKOM"];
immutable int[3] NOTCH = [16, 4, 21];
immutable string REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

int mod26(int a) { return ((a % 26) + 26) % 26; }

struct Rotor {
    string fwd, bwd;
    int notch, offset;

    static Rotor create(int num, char win) {
        return Rotor(FWD[num], BWD[num], NOTCH[num], win - 'A');
    }

    int forwardPass(int idx) {
        int contact = mod26(idx + offset);
        return mod26(fwd[contact] - 'A' - offset);
    }

    int backwardPass(int idx) {
        int contact = mod26(idx + offset);
        return mod26(bwd[contact] - 'A' - offset);
    }

    void step() { offset = (offset + 1) % 26; }
    bool atNotch() { return offset == notch; }
}

struct Enigma {
    Rotor left, middle, right;
    int[26] plug;

    static Enigma create(int[3] rotors, string key, string[] plugboard = []) {
        Enigma e;
        e.left = Rotor.create(rotors[0], key[0]);
        e.middle = Rotor.create(rotors[1], key[1]);
        e.right = Rotor.create(rotors[2], key[2]);
        foreach (i; 0 .. 26) e.plug[i] = i;
        foreach (pair; plugboard) {
            int a = pair[0] - 'A', b = pair[1] - 'A';
            e.plug[a] = b; e.plug[b] = a;
        }
        return e;
    }

    void stepRotors() {
        if (middle.atNotch()) { middle.step(); left.step(); }
        else if (right.atNotch()) { middle.step(); }
        right.step();
    }

    char pressKey(char c) {
        stepRotors();
        int idx = c - 'A';
        idx = plug[idx];
        idx = right.forwardPass(idx);
        idx = middle.forwardPass(idx);
        idx = left.forwardPass(idx);
        idx = REFL[idx] - 'A';
        idx = left.backwardPass(idx);
        idx = middle.backwardPass(idx);
        idx = right.backwardPass(idx);
        idx = plug[idx];
        return cast(char)('A' + idx);
    }

    string encrypt(string text) {
        string result;
        foreach (c; text.toUpper) {
            if (c >= 'A' && c <= 'Z')
                result ~= pressKey(cast(char)c);
        }
        return result;
    }
}

void main() {
    writeln("Enigma Machine - D Implementation");
    writeln("==================================");

    auto tests = [
        tuple([0,1,2], "AAA", cast(string[])[], "AAAAA", "BDZGO"),
        tuple([0,1,2], "AAA", cast(string[])[], "HELLOWORLD", "ILBDAAMTAZ"),
        tuple([0,1,2], "AAA", cast(string[])[], "ATTACKATDAWN", "BZHGNOCRRTCM"),
        tuple([0,1,2], "MCK", cast(string[])[], "HELLOWORLD", "DLTBBQVPQV"),
        tuple([2,0,1], "AAA", cast(string[])[], "HELLOWORLD", "KZHDFQYHXT"),
        tuple([0,1,2], "AAA", ["AB","CD","EF"], "HELLOWORLD", "IKACBBMTBF"),
    ];

    bool allPass = true;
    foreach (i, t; tests) {
        auto e = Enigma.create(t[0], t[1], t[2]);
        auto cipher = e.encrypt(t[3]);
        bool ok = cipher == t[4];
        writefln("  Test %d: %-20s -> %-15s [%s]", i+1, t[3], cipher, ok ? "PASS" : "FAIL");
        if (!ok) { writefln("          Expected %s, got %s", t[4], cipher); allPass = false; }
    }
    writeln(allPass ? "\n  ALL 6 TESTS PASSED" : "\n  SOME TESTS FAILED");
}
