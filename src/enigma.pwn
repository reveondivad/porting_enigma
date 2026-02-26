// Enigma Cipher - Pawn
// Embedded scripting language (game servers, AMX Mod X)

#include <core>
#include <console>
#include <string>

new FWD[3][26] = {
    {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9},
    {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4},
    {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14}
};

new BWD[3][26] = {
    {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9},
    {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18},
    {19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10}
};

new NOTCH[3] = {16, 4, 21};
new REFLECTOR[26] = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};

stock mod26(x) {
    new m = x % 26;
    if (m < 0) m += 26;
    return m;
}

stock pass_fwd(rotor, offset, ch) {
    new inp = mod26(ch + offset);
    new out = FWD[rotor][inp];
    return mod26(out - offset);
}

stock pass_bwd(rotor, offset, ch) {
    new inp = mod26(ch + offset);
    new out = BWD[rotor][inp];
    return mod26(out - offset);
}

stock encrypt(r0, r1, r2, k0, k1, k2, pb[], const msg[], result[], maxlen) {
    new o0 = k0, o1 = k1, o2 = k2;
    new len = strlen(msg);
    for (new i = 0; i < len && i < maxlen - 1; i++) {
        new ch = msg[i] - 'A';
        new bool:mid = (o1 == NOTCH[r1]);
        new bool:atn = (o2 == NOTCH[r2]);
        o2 = mod26(o2 + 1);
        if (atn || mid) o1 = mod26(o1 + 1);
        if (mid) o0 = mod26(o0 + 1);
        new c = pb[ch];
        c = pass_fwd(r2, o2, c);
        c = pass_fwd(r1, o1, c);
        c = pass_fwd(r0, o0, c);
        c = REFLECTOR[c];
        c = pass_bwd(r0, o0, c);
        c = pass_bwd(r1, o1, c);
        c = pass_bwd(r2, o2, c);
        c = pb[c];
        result[i] = c + 'A';
    }
    result[len] = 0;
}

stock make_pb(pb[], pairs[][], npairs) {
    for (new i = 0; i < 26; i++) pb[i] = i;
    for (new i = 0; i < npairs; i++) {
        pb[pairs[i][0]] = pairs[i][1];
        pb[pairs[i][1]] = pairs[i][0];
    }
}

main() {
    new pb[26], result[64];
    printf("Enigma Cipher - Pawn\n");

    make_pb(pb, {}, 0);
    encrypt(0,1,2, 0,0,0, pb, "AAAAA", result, sizeof result);
    printf("Test 1: %s (expected BDZGO)\n", result);

    make_pb(pb, {}, 0);
    encrypt(0,1,2, 0,0,0, pb, "HELLOWORLD", result, sizeof result);
    printf("Test 2: %s (expected ILBDAAMTAZ)\n", result);

    make_pb(pb, {}, 0);
    encrypt(0,1,2, 0,0,0, pb, "ATTACKATDAWN", result, sizeof result);
    printf("Test 3: %s (expected BZHGNOCRRTCM)\n", result);

    make_pb(pb, {}, 0);
    encrypt(0,1,2, 12,2,10, pb, "HELLOWORLD", result, sizeof result);
    printf("Test 4: %s (expected DLTBBQVPQV)\n", result);

    make_pb(pb, {}, 0);
    encrypt(2,0,1, 0,0,0, pb, "HELLOWORLD", result, sizeof result);
    printf("Test 5: %s (expected KZHDFQYHXT)\n", result);

    new pairs[3][2] = {{0,1},{2,3},{4,5}};
    make_pb(pb, pairs, 3);
    encrypt(0,1,2, 0,0,0, pb, "HELLOWORLD", result, sizeof result);
    printf("Test 6: %s (expected IKACBBMTBF)\n", result);
}
