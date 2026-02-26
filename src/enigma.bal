// Enigma Cipher - Ballerina
// Cloud-native programming language for integration

import ballerina/io;

final int[] fwdI = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
final int[] fwdII = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
final int[] fwdIII = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];

final int[] bwdI = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
final int[] bwdII = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
final int[] bwdIII = [19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10];

final int[] notches = [16, 4, 21];
final int[] reflectorB = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];

function mod26(int x) returns int {
    int m = x % 26;
    if m < 0 { m += 26; }
    return m;
}

function getFwd(int r, int i) returns int {
    match r {
        0 => { return fwdI[i]; }
        1 => { return fwdII[i]; }
        _ => { return fwdIII[i]; }
    }
}

function getBwd(int r, int i) returns int {
    match r {
        0 => { return bwdI[i]; }
        1 => { return bwdII[i]; }
        _ => { return bwdIII[i]; }
    }
}

function passFwd(int rotor, int offset, int ch) returns int {
    int inp = mod26(ch + offset);
    int out = getFwd(rotor, inp);
    return mod26(out - offset);
}

function passBwd(int rotor, int offset, int ch) returns int {
    int inp = mod26(ch + offset);
    int out = getBwd(rotor, inp);
    return mod26(out - offset);
}

function makePlugboard(int[][] pairs) returns int[] {
    int[] pb = [];
    foreach int i in 0 ..< 26 { pb.push(i); }
    foreach int[] p in pairs {
        pb[p[0]] = p[1];
        pb[p[1]] = p[0];
    }
    return pb;
}

function encrypt(int r0, int r1, int r2,
                 int k0, int k1, int k2,
                 int[][] pairs, string msg) returns string {
    int[] pb = makePlugboard(pairs);
    int o0 = k0; int o1 = k1; int o2 = k2;
    string result = "";
    foreach int i in 0 ..< msg.length() {
        int ch = <int>msg.getCodePoint(i) - 65;
        boolean mid = o1 == notches[r1];
        boolean atn = o2 == notches[r2];
        o2 = mod26(o2 + 1);
        if atn || mid { o1 = mod26(o1 + 1); }
        if mid { o0 = mod26(o0 + 1); }
        int c = pb[ch];
        c = passFwd(r2, o2, c);
        c = passFwd(r1, o1, c);
        c = passFwd(r0, o0, c);
        c = reflectorB[c];
        c = passBwd(r0, o0, c);
        c = passBwd(r1, o1, c);
        c = passBwd(r2, o2, c);
        c = pb[c];
        string:Char letter = <string:Char>checkpanic string:fromCodePointInt(c + 65);
        result += letter.toString();
    }
    return result;
}

function runTest(string label, string expected, string actual) {
    string status = expected == actual ? "PASS" : "FAIL";
    io:println(status + " " + label + ": " + actual + " (expected " + expected + ")");
}

public function main() {
    io:println("Enigma Cipher - Ballerina");
    int[][] noPairs = [];
    runTest("Test 1", "BDZGO", encrypt(0,1,2, 0,0,0, noPairs, "AAAAA"));
    runTest("Test 2", "ILBDAAMTAZ", encrypt(0,1,2, 0,0,0, noPairs, "HELLOWORLD"));
    runTest("Test 3", "BZHGNOCRRTCM", encrypt(0,1,2, 0,0,0, noPairs, "ATTACKATDAWN"));
    runTest("Test 4", "DLTBBQVPQV", encrypt(0,1,2, 12,2,10, noPairs, "HELLOWORLD"));
    runTest("Test 5", "KZHDFQYHXT", encrypt(2,0,1, 0,0,0, noPairs, "HELLOWORLD"));
    runTest("Test 6", "IKACBBMTBF", encrypt(0,1,2, 0,0,0, [[0,1],[2,3],[4,5]], "HELLOWORLD"));
}
