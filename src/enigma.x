// Enigma cipher in X language (Dlang-inspired systems language)
module enigma;

immutable int[26] rotorFwd1 = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
immutable int[26] rotorFwd2 = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
immutable int[26] rotorFwd3 = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];
immutable int[26] rotorBwd1 = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
immutable int[26] rotorBwd2 = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
immutable int[26] rotorBwd3 = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12];
immutable int[26] reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
immutable int[3] notches = [16, 4, 21];

int mod26(int n) { return ((n % 26) + 26) % 26; }

int rotorPass(ref immutable int[26] wiring, int c, int pos) {
    return mod26(wiring[mod26(c + pos)] - pos);
}

string enigma(string text) {
    int[3] pos = [0, 0, 0];
    string result;
    foreach (ch; text) {
        int c = cast(int)(ch.toUpper) - 65;
        if (c < 0 || c > 25) continue;
        bool mid = pos[1] == notches[1];
        if (pos[2] == notches[2]) pos[2] = mod26(pos[2] + 1);
        if (mid || pos[2] == notches[2]) pos[1] = mod26(pos[1] + 1);
        pos[2] = mod26(pos[2] + 1);
        immutable fwd = [&rotorFwd1, &rotorFwd2, &rotorFwd3];
        immutable bwd = [&rotorBwd1, &rotorBwd2, &rotorBwd3];
        for (int i = 2; i >= 0; i--) c = rotorPass(*fwd[i], c, pos[i]);
        c = reflector[c];
        for (int i = 0; i <= 2; i++) c = rotorPass(*bwd[i], c, pos[i]);
        result ~= cast(char)(c + 65);
    }
    return result;
}

void main() { writeln(enigma("HELLOWORLD")); }
