# Enigma Cipher - Cython
# Python with C-level performance via type declarations
# cython: language_level=3

cdef int FWD[3][26]
cdef int BWD[3][26]
cdef int NOTCH[3]
cdef int REFLECTOR[26]

cdef void init_wirings():
    cdef int fI[26], fII[26], fIII[26]
    cdef int bI[26], bII[26], bIII[26]
    cdef int refl[26]
    
    fI[:] = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
    fII[:] = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
    fIII[:] = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
    
    bI[:] = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
    bII[:] = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
    bIII[:] = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
    
    refl[:] = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
    
    for i in range(26):
        FWD[0][i] = fI[i]; FWD[1][i] = fII[i]; FWD[2][i] = fIII[i]
        BWD[0][i] = bI[i]; BWD[1][i] = bII[i]; BWD[2][i] = bIII[i]
        REFLECTOR[i] = refl[i]
    
    NOTCH[0] = 16; NOTCH[1] = 4; NOTCH[2] = 21

cdef int mod26(int x):
    cdef int m = x % 26
    if m < 0:
        m += 26
    return m

cdef int pass_fwd(int rotor, int offset, int ch):
    cdef int inp = mod26(ch + offset)
    cdef int out = FWD[rotor][inp]
    return mod26(out - offset)

cdef int pass_bwd(int rotor, int offset, int ch):
    cdef int inp = mod26(ch + offset)
    cdef int out = BWD[rotor][inp]
    return mod26(out - offset)

def encrypt(int r0, int r1, int r2, int k0, int k1, int k2, list pairs, str msg):
    cdef int pb[26]
    cdef int o0 = k0, o1 = k1, o2 = k2
    cdef int ch, c, i
    cdef bint mid, atn
    
    for i in range(26):
        pb[i] = i
    for a, b in pairs:
        pb[a] = b
        pb[b] = a
    
    result = []
    for char in msg:
        ch = ord(char) - 65
        mid = (o1 == NOTCH[r1])
        atn = (o2 == NOTCH[r2])
        o2 = mod26(o2 + 1)
        if atn or mid:
            o1 = mod26(o1 + 1)
        if mid:
            o0 = mod26(o0 + 1)
        
        c = pb[ch]
        c = pass_fwd(r2, o2, c)
        c = pass_fwd(r1, o1, c)
        c = pass_fwd(r0, o0, c)
        c = REFLECTOR[c]
        c = pass_bwd(r0, o0, c)
        c = pass_bwd(r1, o1, c)
        c = pass_bwd(r2, o2, c)
        c = pb[c]
        result.append(chr(c + 65))
    
    return ''.join(result)

init_wirings()

def run_test(label, expected, r0, r1, r2, k0, k1, k2, pairs, msg):
    actual = encrypt(r0, r1, r2, k0, k1, k2, pairs, msg)
    status = "PASS" if actual == expected else "FAIL"
    print(f"{status} {label}: {actual} (expected {expected})")

if __name__ == "__main__":
    print("Enigma Cipher - Cython")
    run_test("Test 1", "BDZGO", 0,1,2, 0,0,0, [], "AAAAA")
    run_test("Test 2", "ILBDAAMTAZ", 0,1,2, 0,0,0, [], "HELLOWORLD")
    run_test("Test 3", "BZHGNOCRRTCM", 0,1,2, 0,0,0, [], "ATTACKATDAWN")
    run_test("Test 4", "DLTBBQVPQV", 0,1,2, 12,2,10, [], "HELLOWORLD")
    run_test("Test 5", "KZHDFQYHXT", 2,0,1, 0,0,0, [], "HELLOWORLD")
    run_test("Test 6", "IKACBBMTBF", 0,1,2, 0,0,0, [(0,1),(2,3),(4,5)], "HELLOWORLD")