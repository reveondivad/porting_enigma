// Enigma Cipher - Metal Shading Language (Apple GPU)
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

#include <metal_stdlib>
using namespace metal;

constant int fwdI[26]  = {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9};
constant int fwdII[26] = {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4};
constant int fwdIII[26]= {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14};
constant int bwdI[26]  = {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9};
constant int bwdII[26] = {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18};
constant int bwdIII[26]= {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12};
constant int ref[26]   = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
constant int notch_pos[3] = {16, 4, 21};

int mod26(int n) { int m = n % 26; return m < 0 ? m + 26 : m; }
int getFwd(int r, int i) { return r==0 ? fwdI[i] : r==1 ? fwdII[i] : fwdIII[i]; }
int getBwd(int r, int i) { return r==0 ? bwdI[i] : r==1 ? bwdII[i] : bwdIII[i]; }
int passFwd(int rotor, int offset, int ch) { return mod26(getFwd(rotor, mod26(ch+offset)) - offset); }
int passBwd(int rotor, int offset, int ch) { return mod26(getBwd(rotor, mod26(ch+offset)) - offset); }

struct Config { int r0; int r1; int r2; int k0; int k1; int k2; int len; };

kernel void enigma_encrypt(
    device const int* input [[buffer(0)]],
    device int* output [[buffer(1)]],
    constant Config& cfg [[buffer(2)]],
    uint gid [[thread_position_in_grid]])
{
    // Sequential - run single thread for correct stepping
    if (gid != 0) return;

    int o0=cfg.k0, o1=cfg.k1, o2=cfg.k2;
    int n1=notch_pos[cfg.r1], n2=notch_pos[cfg.r2];

    for (int i = 0; i < cfg.len; i++) {
        bool mid = (o1 == n1), atn = (o2 == n2);
        o2 = mod26(o2+1);
        if (atn || mid) o1 = mod26(o1+1);
        if (mid) o0 = mod26(o0+1);

        int c = input[i];
        c = passFwd(cfg.r2,o2,c); c = passFwd(cfg.r1,o1,c); c = passFwd(cfg.r0,o0,c);
        c = ref[c];
        c = passBwd(cfg.r0,o0,c); c = passBwd(cfg.r1,o1,c); c = passBwd(cfg.r2,o2,c);
        output[i] = c;
    }
}
