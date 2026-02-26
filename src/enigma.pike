// Enigma Machine - Pike Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

array(string) FWD = ({"EKMFLGDQVZNTOWYHXUSPAIBRCJ",
                       "AJDKSIRUXBLHWTMCQGZNPYFVOE",
                       "BDFHJLCPRTXVZNYEIWGAKMUSQO"});
array(string) BWD = ({"UWYGADFPVZBECKMTHXSLRINQOJ",
                       "AJPCZWRLFBDKOTYUQGENHXMIVS",
                       "TAGBPCSDQEUFVNZHYIXJWLRKOM"});
array(int) NOTCH = ({16, 4, 21});
string REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

int mod26(int a) { return ((a % 26) + 26) % 26; }

class Rotor {
    string fwd, bwd;
    int notch, offset;
    void create(int num, int win) {
        fwd = FWD[num]; bwd = BWD[num];
        notch = NOTCH[num]; offset = win - 'A';
    }
    int forward(int idx) {
        int contact = mod26(idx + offset);
        return mod26(fwd[contact] - 'A' - offset);
    }
    int backward(int idx) {
        int contact = mod26(idx + offset);
        return mod26(bwd[contact] - 'A' - offset);
    }
    void step() { offset = (offset + 1) % 26; }
    int at_notch() { return offset == notch; }
}

class Enigma {
    Rotor left, middle, right;
    array(int) plug = allocate(26);

    void create(array(int) rotors, string key, array(string)|void plugboard) {
        left = Rotor(rotors[0], key[0]);
        middle = Rotor(rotors[1], key[1]);
        right = Rotor(rotors[2], key[2]);
        for (int i = 0; i < 26; i++) plug[i] = i;
        if (plugboard) {
            foreach (plugboard, string pair) {
                int a = pair[0] - 'A', b = pair[1] - 'A';
                plug[a] = b; plug[b] = a;
            }
        }
    }

    void step_rotors() {
        if (middle->at_notch()) { middle->step(); left->step(); }
        else if (right->at_notch()) { middle->step(); }
        right->step();
    }

    int press_key(int c) {
        step_rotors();
        int idx = c - 'A';
        idx = plug[idx];
        idx = right->forward(idx);
        idx = middle->forward(idx);
        idx = left->forward(idx);
        idx = REFL[idx] - 'A';
        idx = left->backward(idx);
        idx = middle->backward(idx);
        idx = right->backward(idx);
        idx = plug[idx];
        return idx + 'A';
    }

    string encrypt(string text) {
        string result = "";
        foreach (upper_case(text); ; int c) {
            if (c >= 'A' && c <= 'Z') result += sprintf("%c", press_key(c));
        }
        return result;
    }
}

int main() {
    write("Enigma Machine - Pike Implementation\n");
    write("=====================================\n");

    array tests = ({
        ({({0,1,2}), "AAA", 0,              "AAAAA",        "BDZGO"}),
        ({({0,1,2}), "AAA", 0,              "HELLOWORLD",   "ILBDAAMTAZ"}),
        ({({0,1,2}), "AAA", 0,              "ATTACKATDAWN", "BZHGNOCRRTCM"}),
        ({({0,1,2}), "MCK", 0,              "HELLOWORLD",   "DLTBBQVPQV"}),
        ({({2,0,1}), "AAA", 0,              "HELLOWORLD",   "KZHDFQYHXT"}),
        ({({0,1,2}), "AAA", ({"AB","CD","EF"}), "HELLOWORLD", "IKACBBMTBF"})
    });

    int all_pass = 1;
    for (int i = 0; i < sizeof(tests); i++) {
        array t = tests[i];
        Enigma e = Enigma(t[0], t[1], t[2] ? t[2] : 0);
        string cipher = e->encrypt(t[3]);
        int ok = cipher == t[4];
        write("  Test %d: %-20s -> %-15s [%s]\n", i+1, t[3], cipher, ok ? "PASS" : "FAIL");
        if (!ok) { write("          Expected %s\n", t[4]); all_pass = 0; }
    }
    write(all_pass ? "\n  ALL 6 TESTS PASSED\n" : "\n  SOME TESTS FAILED\n");
    return 0;
}
