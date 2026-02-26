// Enigma Machine - Vala Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

const string[] FWD = {"EKMFLGDQVZNTOWYHXUSPAIBRCJ",
                       "AJDKSIRUXBLHWTMCQGZNPYFVOE",
                       "BDFHJLCPRTXVZNYEIWGAKMUSQO"};
const string[] BWD = {"UWYGADFPVZBECKMTHXSLRINQOJ",
                       "AJPCZWRLFBDKOTYUQGENHXMIVS",
                       "TAGBPCSDQEUFVNZHYIXJWLRKOM"};
const int[] NOTCH = {16, 4, 21};
const string REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

int mod26(int a) { return ((a % 26) + 26) % 26; }

class Rotor {
    public string fwd; public string bwd;
    public int notch; public int offset;
    public Rotor(int num, char win) {
        fwd = FWD[num]; bwd = BWD[num];
        notch = NOTCH[num]; offset = win - 'A';
    }
    public int forward(int idx) {
        int contact = mod26(idx + offset);
        return mod26(fwd[contact] - 'A' - offset);
    }
    public int backward(int idx) {
        int contact = mod26(idx + offset);
        return mod26(bwd[contact] - 'A' - offset);
    }
    public void step() { offset = (offset + 1) % 26; }
    public bool at_notch() { return offset == notch; }
}

class Enigma {
    Rotor left; Rotor middle; Rotor right;
    int[] plug = new int[26];

    public Enigma(int[] rotors, string key, string[]? plugboard) {
        left = new Rotor(rotors[0], key[0]);
        middle = new Rotor(rotors[1], key[1]);
        right = new Rotor(rotors[2], key[2]);
        for (int i = 0; i < 26; i++) plug[i] = i;
        if (plugboard != null) {
            foreach (string pair in plugboard) {
                int a = pair[0] - 'A', b = pair[1] - 'A';
                plug[a] = b; plug[b] = a;
            }
        }
    }

    void step_rotors() {
        if (middle.at_notch()) { middle.step(); left.step(); }
        else if (right.at_notch()) { middle.step(); }
        right.step();
    }

    char press_key(char c) {
        step_rotors();
        int idx = c - 'A';
        idx = plug[idx];
        idx = right.forward(idx);
        idx = middle.forward(idx);
        idx = left.forward(idx);
        idx = REFL[idx] - 'A';
        idx = left.backward(idx);
        idx = middle.backward(idx);
        idx = right.backward(idx);
        idx = plug[idx];
        return (char)('A' + idx);
    }

    public string encrypt(string text) {
        var sb = new StringBuilder();
        foreach (char c in text.up().data) {
            if (c >= 'A' && c <= 'Z') sb.append_c(press_key(c));
        }
        return sb.str;
    }
}

void main() {
    print("Enigma Machine - Vala Implementation\n");
    print("=====================================\n");

    struct Test { int[] rotors; string key; string[]? plugs; string plain; string expected; }
    Test[] tests = {
        {{0,1,2}, "AAA", null,              "AAAAA",        "BDZGO"},
        {{0,1,2}, "AAA", null,              "HELLOWORLD",   "ILBDAAMTAZ"},
        {{0,1,2}, "AAA", null,              "ATTACKATDAWN", "BZHGNOCRRTCM"},
        {{0,1,2}, "MCK", null,              "HELLOWORLD",   "DLTBBQVPQV"},
        {{2,0,1}, "AAA", null,              "HELLOWORLD",   "KZHDFQYHXT"},
        {{0,1,2}, "AAA", {"AB","CD","EF"},  "HELLOWORLD",   "IKACBBMTBF"}
    };

    bool all_pass = true;
    for (int i = 0; i < tests.length; i++) {
        var t = tests[i];
        var e = new Enigma(t.rotors, t.key, t.plugs);
        string cipher = e.encrypt(t.plain);
        bool ok = cipher == t.expected;
        print("  Test %d: %-20s -> %-15s [%s]\n", i+1, t.plain, cipher, ok ? "PASS" : "FAIL");
        if (!ok) { print("          Expected %s\n", t.expected); all_pass = false; }
    }
    print(all_pass ? "\n  ALL 6 TESTS PASSED\n" : "\n  SOME TESTS FAILED\n");
}
