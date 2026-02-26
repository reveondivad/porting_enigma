// Enigma Cipher - Ring
// Practical general-purpose programming language

fwdI = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
fwdII = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
fwdIII = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]

bwdI = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
bwdII = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
bwdIII = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]

notches = [16, 4, 21]
reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]

func mod26 x
    m = x % 26
    if m < 0 m += 26 ok
    return m

func getFwd r, i
    switch r
        on 0 return fwdI[i+1]
        on 1 return fwdII[i+1]
        on 2 return fwdIII[i+1]
    off

func getBwd r, i
    switch r
        on 0 return bwdI[i+1]
        on 1 return bwdII[i+1]
        on 2 return bwdIII[i+1]
    off

func passFwd rotor, offset, ch
    inp = mod26(ch + offset)
    out = getFwd(rotor, inp)
    return mod26(out - offset)

func passBwd rotor, offset, ch
    inp = mod26(ch + offset)
    out = getBwd(rotor, inp)
    return mod26(out - offset)

func makePlugboard pairs
    pb = list(26)
    for i = 1 to 26 pb[i] = i - 1 next
    for p in pairs
        a = p[1] + 1
        b = p[2] + 1
        pb[a] = p[2]
        pb[b] = p[1]
    next
    return pb

func encrypt r0, r1, r2, k0, k1, k2, pairs, msg
    pb = makePlugboard(pairs)
    o0 = k0  o1 = k1  o2 = k2
    result = ""
    for i = 1 to len(msg)
        ch = ascii(msg[i]) - ascii("A")
        mid = (o1 = notches[r1+1])
        atn = (o2 = notches[r2+1])
        o2 = mod26(o2 + 1)
        if atn or mid o1 = mod26(o1 + 1) ok
        if mid o0 = mod26(o0 + 1) ok
        c = pb[ch + 1]
        c = passFwd(r2, o2, c)
        c = passFwd(r1, o1, c)
        c = passFwd(r0, o0, c)
        c = reflector[c + 1]
        c = passBwd(r0, o0, c)
        c = passBwd(r1, o1, c)
        c = passBwd(r2, o2, c)
        c = pb[c + 1]
        result += char(c + ascii("A"))
    next
    return result

func runTest label, expected, actual
    if expected = actual status = "PASS"
    else status = "FAIL" ok
    see status + " " + label + ": " + actual + " (expected " + expected + ")" + nl

see "Enigma Cipher - Ring" + nl
runTest("Test 1", "BDZGO", encrypt(0,1,2, 0,0,0, [], "AAAAA"))
runTest("Test 2", "ILBDAAMTAZ", encrypt(0,1,2, 0,0,0, [], "HELLOWORLD"))
runTest("Test 3", "BZHGNOCRRTCM", encrypt(0,1,2, 0,0,0, [], "ATTACKATDAWN"))
runTest("Test 4", "DLTBBQVPQV", encrypt(0,1,2, 12,2,10, [], "HELLOWORLD"))
runTest("Test 5", "KZHDFQYHXT", encrypt(2,0,1, 0,0,0, [], "HELLOWORLD"))
runTest("Test 6", "IKACBBMTBF", encrypt(0,1,2, 0,0,0, [[0,1],[2,3],[4,5]], "HELLOWORLD"))