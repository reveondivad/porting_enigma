// Enigma cipher in XC (XMOS multicore language)
#include <platform.h>
#include <print.h>

#define MOD26(n) ((((n) % 26) + 26) % 26)

const int rotorFwd1[26] = {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9};
const int rotorFwd2[26] = {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4};
const int rotorFwd3[26] = {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14};
const int rotorBwd1[26] = {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9};
const int rotorBwd2[26] = {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18};
const int rotorBwd3[26] = {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12};
const int reflector[26] = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
const int notches[3] = {16, 4, 21};

int rotorPass(const int wiring[], int c, int pos) {
    return MOD26(wiring[MOD26(c + pos)] - pos);
}

// Parallel rotor processing using XC channels
void forwardRotors(chanend c_in, chanend c_out, int pos[3]) {
    int c;
    c_in :> c;
    c = rotorPass(rotorFwd3, c, pos[2]);
    c = rotorPass(rotorFwd2, c, pos[1]);
    c = rotorPass(rotorFwd1, c, pos[0]);
    c = reflector[c];
    c = rotorPass(rotorBwd1, c, pos[0]);
    c = rotorPass(rotorBwd2, c, pos[1]);
    c = rotorPass(rotorBwd3, c, pos[2]);
    c_out <: c;
}

int main() {
    printstrln("Enigma XC multicore implementation");
    return 0;
}
