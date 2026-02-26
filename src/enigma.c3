// Enigma Cipher - C3
// Evolution of C with modern features, no preprocessor
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

module enigma;
import std::io;

const int[26][3] FWD = {
    {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9},
    {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4},
    {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14},
};
const int[26][3] BWD = {
    {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9},
    {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18},
    {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12},
};
const int[26] REF = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
const int[3] NOTCH = {16, 4, 21};

fn int mod26(int n) {
    int m = n % 26;
    return m < 0 ? m + 26 : m;
}

struct Enigma {
    int[3] r;
    int[3] o;
    int n1;
    int n2;
}

fn Enigma new_enigma(int r0, int r1, int r2, int k0, int k1, int k2) {
    return Enigma {
        .r = {r0, r1, r2},
        .o = {k0, k1, k2},
        .n1 = NOTCH[r1],
        .n2 = NOTCH[r2],
    };
}

fn void Enigma.step(&self) {
    if (self.o[1] == self.n1) {
        self.o[1] = mod26(self.o[1] + 1);
        self.o[0] = mod26(self.o[0] + 1);
    } else if (self.o[2] == self.n2) {
        self.o[1] = mod26(self.o[1] + 1);
    }
    self.o[2] = mod26(self.o[2] + 1);
}

fn int Enigma.fwd_pass(&self, int slot, int idx) {
    int c = mod26(idx + self.o[slot]);
    int out = FWD[self.r[slot]][c];
    return mod26(out - self.o[slot]);
}

fn int Enigma.bwd_pass(&self, int slot, int idx) {
    int c = mod26(idx + self.o[slot]);
    int out = BWD[self.r[slot]][c];
    return mod26(out - self.o[slot]);
}

fn int Enigma.press_key(&self, int ch) {
    self.step();
    int c = ch;
    c = self.fwd_pass(2, c); c = self.fwd_pass(1, c); c = self.fwd_pass(0, c);
    c = REF[c];
    c = self.bwd_pass(0, c); c = self.bwd_pass(1, c); c = self.bwd_pass(2, c);
    return c;
}

fn void main() {
    io::printn("Enigma Cipher - C3");
    Enigma e = new_enigma(0,1,2,0,0,0);
    char[64] buf;
    String msg = "AAAAA";
    for (int i = 0; i < msg.len; i++) {
        buf[i] = (char)(e.press_key(msg[i] - 'A') + 'A');
    }
    buf[msg.len] = 0;
    io::printfn("Test 1: %s expected BDZGO", &buf);
}
