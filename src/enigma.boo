# Enigma Cipher - Boo
# Python-inspired language for .NET CLR

import System

fwdI = (4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9)
fwdII = (0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4)
fwdIII = (1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14)

bwdI = (20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9)
bwdII = (0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18)
bwdIII = (19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10)

notches = (16, 4, 21)
reflectorB = (24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19)

fwd = (fwdI, fwdII, fwdIII)
bwd = (bwdI, bwdII, bwdIII)

def mod26(x as int) as int:
    m = x % 26
    return (m + 26) % 26

def passFwd(rotor as int, offset as int, ch as int) as int:
    inp = mod26(ch + offset)
    out = fwd[rotor][inp]
    return mod26(out - offset)

def passBwd(rotor as int, offset as int, ch as int) as int:
    inp = mod26(ch + offset)
    out = bwd[rotor][inp]
    return mod26(out - offset)

def makePlugboard(pairs as ((int,),)):
    pb = array(int, 26)
    for i in range(26):
        pb[i] = i
    for p in pairs:
        pb[p[0]] = p[1]
        pb[p[1]] = p[0]
    return pb

def encrypt(r0 as int, r1 as int, r2 as int,
            k0 as int, k1 as int, k2 as int,
            pairs, msg as string) as string:
    pb = makePlugboard(pairs)
    o0 = k0; o1 = k1; o2 = k2
    result = ""
    for ch as char in msg:
        c = cast(int, ch) - cast(int, char('A'))
        mid = (o1 == notches[r1])
        atn = (o2 == notches[r2])
        o2 = mod26(o2 + 1)
        if atn or mid:
            o1 = mod26(o1 + 1)
        if mid:
            o0 = mod26(o0 + 1)
        v = pb[c]
        v = passFwd(r2, o2, v)
        v = passFwd(r1, o1, v)
        v = passFwd(r0, o0, v)
        v = reflectorB[v]
        v = passBwd(r0, o0, v)
        v = passBwd(r1, o1, v)
        v = passBwd(r2, o2, v)
        v = pb[v]
        result += char(v + cast(int, char('A')))
    return result

def runTest(label as string, expected as string, actual as string):
    status = ("PASS" if expected == actual else "FAIL")
    print "${status} ${label}: ${actual} (expected ${expected})"

print "Enigma Cipher - Boo"
noPairs = ()
runTest("Test 1", "BDZGO", encrypt(0,1,2, 0,0,0, noPairs, "AAAAA"))
runTest("Test 2", "ILBDAAMTAZ", encrypt(0,1,2, 0,0,0, noPairs, "HELLOWORLD"))
runTest("Test 3", "BZHGNOCRRTCM", encrypt(0,1,2, 0,0,0, noPairs, "ATTACKATDAWN"))
runTest("Test 4", "DLTBBQVPQV", encrypt(0,1,2, 12,2,10, noPairs, "HELLOWORLD"))
runTest("Test 5", "KZHDFQYHXT", encrypt(2,0,1, 0,0,0, noPairs, "HELLOWORLD"))
runTest("Test 6", "IKACBBMTBF", encrypt(0,1,2, 0,0,0, ((0,1),(2,3),(4,5)), "HELLOWORLD"))
