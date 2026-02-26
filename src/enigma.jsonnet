// Enigma Cipher - Jsonnet
// Data templating language (Google)
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

local fwd = [
    [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
    [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
    [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14],
];
local bwd = [
    [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
    [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
    [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12],
];
local reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
local notches = [16, 4, 21];

local mod26(n) = local m = n % 26; if m < 0 then m + 26 else m;

local passFwd(rotor, offset, ch) =
    local inp = mod26(ch + offset);
    local out = fwd[rotor][inp];
    mod26(out - offset);

local passBwd(rotor, offset, ch) =
    local inp = mod26(ch + offset);
    local out = bwd[rotor][inp];
    mod26(out - offset);

// Recursive encryption with state threading
local encryptHelper(r, o, n1, n2, chars, idx) =
    if idx >= std.length(chars) then []
    else
        // Step
        local mid = o[1] == n1;
        local atn = o[2] == n2;
        local newO2 = mod26(o[2] + 1);
        local newO1 = if mid || atn then mod26(o[1] + 1) else o[1];
        local newO0 = if mid then mod26(o[0] + 1) else o[0];
        local newO = [newO0, newO1, newO2];
        // Forward
        local c = chars[idx];
        local c1 = passFwd(r[2], newO[2], c);
        local c2 = passFwd(r[1], newO[1], c1);
        local c3 = passFwd(r[0], newO[0], c2);
        local c4 = reflector[c3];
        // Backward
        local c5 = passBwd(r[0], newO[0], c4);
        local c6 = passBwd(r[1], newO[1], c5);
        local c7 = passBwd(r[2], newO[2], c6);
        [c7] + encryptHelper(r, newO, n1, n2, chars, idx + 1);

local encrypt(r0, r1, r2, k0, k1, k2, msg) =
    local chars = [std.codepoint(msg[i]) - 65 for i in std.range(0, std.length(msg))];
    local result = encryptHelper([r0,r1,r2], [k0,k1,k2], notches[r1], notches[r2], chars, 0);
    std.join("", [std.char(c + 65) for c in result]);

{
    title: "Enigma Cipher - Jsonnet",
    test1: { input: "AAAAA",        output: encrypt(0,1,2,0,0,0, "AAAAA"),        expected: "BDZGO" },
    test2: { input: "HELLOWORLD",   output: encrypt(0,1,2,0,0,0, "HELLOWORLD"),   expected: "ILBDAAMTAZ" },
    test3: { input: "ATTACKATDAWN", output: encrypt(0,1,2,0,0,0, "ATTACKATDAWN"), expected: "BZHGNOCRRTCM" },
    test4: { input: "HELLOWORLD",   output: encrypt(0,1,2,12,2,10, "HELLOWORLD"), expected: "DLTBBQVPQV" },
    test5: { input: "HELLOWORLD",   output: encrypt(2,0,1,0,0,0, "HELLOWORLD"),   expected: "KZHDFQYHXT" },
}
