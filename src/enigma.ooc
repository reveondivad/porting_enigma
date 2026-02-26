// Enigma cipher in ooc (object-oriented C)
import structs/ArrayList

mod26: func (n: Int) -> Int { ((n % 26) + 26) % 26 }

rotorFwd := [
    [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9] as Int[],
    [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4] as Int[],
    [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14] as Int[]
] as Int[][]

rotorBwd := [
    [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9] as Int[],
    [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18] as Int[],
    [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12] as Int[]
] as Int[][]

reflector := [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19] as Int[]
notches := [16, 4, 21] as Int[]

rotorPass: func (wiring: Int[], c: Int, pos: Int) -> Int {
    mod26(wiring[mod26(c + pos)] - pos)
}

enigma: func (text: String) -> String {
    pos := [0, 0, 0] as Int[]
    result := Buffer new()
    for (i in 0..text size) {
        c := text[i] toUpper() - 'A'
        if (c < 0 || c > 25) continue
        mid := pos[1] == notches[1]
        if (pos[2] == notches[2]) pos[2] = mod26(pos[2] + 1)
        if (mid || pos[2] == notches[2]) pos[1] = mod26(pos[1] + 1)
        pos[2] = mod26(pos[2] + 1)
        for (j in 0..3) { idx := 2 - j; c = rotorPass(rotorFwd[idx], c, pos[idx]) }
        c = reflector[c]
        for (j in 0..3) c = rotorPass(rotorBwd[j], c, pos[j])
        result append((c + 'A') as Char)
    }
    result toString()
}

main: func { enigma("HELLOWORLD") println() }
