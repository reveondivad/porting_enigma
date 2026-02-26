-- Enigma Cipher - Terra
-- Low-level systems language embedded in Lua

local C = terralib.includec("stdio.h")

local FWD = global(int[3][26])
local BWD = global(int[3][26])
local NOTCH = global(int[3])
local REFLECTOR = global(int[26])

terra initWirings()
    var fI = array(4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9)
    var fII = array(0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4)
    var fIII = array(1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14)

    var bI = array(20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9)
    var bII = array(0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18)
    var bIII = array(19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12)

    for i = 0, 26 do
        FWD[0][i] = fI[i]; FWD[1][i] = fII[i]; FWD[2][i] = fIII[i]
        BWD[0][i] = bI[i]; BWD[1][i] = bII[i]; BWD[2][i] = bIII[i]
    end

    NOTCH[0] = 16; NOTCH[1] = 4; NOTCH[2] = 21

    var refl = array(24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19)
    for i = 0, 26 do REFLECTOR[i] = refl[i] end
end

terra mod26(x: int): int
    var m = x % 26
    if m < 0 then m = m + 26 end
    return m
end

terra passFwd(rotor: int, offset: int, ch: int): int
    var inp = mod26(ch + offset)
    var out = FWD[rotor][inp]
    return mod26(out - offset)
end

terra passBwd(rotor: int, offset: int, ch: int): int
    var inp = mod26(ch + offset)
    var out = BWD[rotor][inp]
    return mod26(out - offset)
end

terra encrypt(r0: int, r1: int, r2: int,
              k0: int, k1: int, k2: int,
              pb: &int, msg: &int8, result: &int8, len: int)
    var o0, o1, o2 = k0, k1, k2
    for i = 0, len do
        var ch = msg[i] - 65
        var mid = (o1 == NOTCH[r1])
        var atn = (o2 == NOTCH[r2])
        o2 = mod26(o2 + 1)
        if atn or mid then o1 = mod26(o1 + 1) end
        if mid then o0 = mod26(o0 + 1) end
        var c = pb[ch]
        c = passFwd(r2, o2, c)
        c = passFwd(r1, o1, c)
        c = passFwd(r0, o0, c)
        c = REFLECTOR[c]
        c = passBwd(r0, o0, c)
        c = passBwd(r1, o1, c)
        c = passBwd(r2, o2, c)
        c = pb[c]
        result[i] = c + 65
    end
    result[len] = 0
end

terra makePB(pb: &int)
    for i = 0, 26 do pb[i] = i end
end

terra setPBPair(pb: &int, a: int, b: int)
    pb[a] = b; pb[b] = a
end

terra runTest(label: &int8, expected: &int8, actual: &int8)
    var match = true
    var i = 0
    while expected[i] ~= 0 and actual[i] ~= 0 do
        if expected[i] ~= actual[i] then match = false end
        i = i + 1
    end
    if expected[i] ~= actual[i] then match = false end
    if match then C.printf("PASS ") else C.printf("FAIL ") end
    C.printf("%s: %s (expected %s)\n", label, actual, expected)
end

terra main()
    initWirings()
    C.printf("Enigma Cipher - Terra\n")

    var pb: int[26]
    var result: int8[64]

    makePB(&pb[0]); encrypt(0,1,2, 0,0,0, &pb[0], "AAAAA", &result[0], 5)
    runTest("Test 1", "BDZGO", &result[0])

    makePB(&pb[0]); encrypt(0,1,2, 0,0,0, &pb[0], "HELLOWORLD", &result[0], 10)
    runTest("Test 2", "ILBDAAMTAZ", &result[0])

    makePB(&pb[0]); encrypt(0,1,2, 0,0,0, &pb[0], "ATTACKATDAWN", &result[0], 12)
    runTest("Test 3", "BZHGNOCRRTCM", &result[0])

    makePB(&pb[0]); encrypt(0,1,2, 12,2,10, &pb[0], "HELLOWORLD", &result[0], 10)
    runTest("Test 4", "DLTBBQVPQV", &result[0])

    makePB(&pb[0]); encrypt(2,0,1, 0,0,0, &pb[0], "HELLOWORLD", &result[0], 10)
    runTest("Test 5", "KZHDFQYHXT", &result[0])

    makePB(&pb[0]); setPBPair(&pb[0],0,1); setPBPair(&pb[0],2,3); setPBPair(&pb[0],4,5)
    encrypt(0,1,2, 0,0,0, &pb[0], "HELLOWORLD", &result[0], 10)
    runTest("Test 6", "IKACBBMTBF", &result[0])
end

main()
