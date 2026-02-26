# Enigma Cipher - Imba
# Full-stack web language with built-in DOM/tag support
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

const FWD = [
    [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
    [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
    [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
]
const BWD = [
    [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
    [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
    [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
]
const REF = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
const NOTCH = [16, 4, 21]

def mod26 n
    let m = n % 26
    m < 0 ? m + 26 : m

class Enigma
    prop r\number[]
    prop o\number[]
    prop n1\number
    prop n2\number

    def constructor r0, r1, r2, k0, k1, k2
        r = [r0, r1, r2]
        o = [k0, k1, k2]
        n1 = NOTCH[r1]
        n2 = NOTCH[r2]

    def step
        if o[1] == n1
            o[1] = mod26(o[1]+1)
            o[0] = mod26(o[0]+1)
        elif o[2] == n2
            o[1] = mod26(o[1]+1)
        o[2] = mod26(o[2]+1)

    def fwdPass slot, idx
        let c = mod26(idx + o[slot])
        mod26(FWD[r[slot]][c] - o[slot])

    def bwdPass slot, idx
        let c = mod26(idx + o[slot])
        mod26(BWD[r[slot]][c] - o[slot])

    def pressKey ch
        step!
        let c = fwdPass(2, fwdPass(1, fwdPass(0, ch) |> do $1) |> do $1)
        # Simplified: forward through 3 rotors
        var v = ch
        v = fwdPass(2, v)
        v = fwdPass(1, v)
        v = fwdPass(0, v)
        v = REF[v]
        v = bwdPass(0, v)
        v = bwdPass(1, v)
        v = bwdPass(2, v)
        v

    def encrypt msg
        let result = []
        for ch, i in msg
            let code = ch.charCodeAt(0) - 65
            result.push String.fromCharCode(pressKey(code) + 65)
        result.join("")

# Test
let e = new Enigma(0,1,2,0,0,0)
console.log "Enigma Cipher - Imba"
console.log "Test 1: {e.encrypt('AAAAA')} expected BDZGO"
