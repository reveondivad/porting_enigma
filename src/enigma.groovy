// Enigma Machine - Groovy Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

class Enigma {
    static final String[] FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"]
    static final String[] BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"]
    static final String REF = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
    static final int[] NOTCH = [16, 4, 21] // Q, E, V

    int[] rotors = new int[3]
    int[] offsets = new int[3]
    int[] notches = new int[3]
    int[] plugboard = new int[26]
    String[][] fwd = new String[3][]
    String[][] bwd = new String[3][]

    Enigma(int r1, int r2, int r3, char k1, char k2, char k3, String plugPairs = "") {
        int[] ri = [r1-1, r2-1, r3-1]
        rotors = ri
        offsets = [(int)(k1 - 'A'), (int)(k2 - 'A'), (int)(k3 - 'A')]
        notches = [NOTCH[ri[0]], NOTCH[ri[1]], NOTCH[ri[2]]]
        for (int i = 0; i < 26; i++) plugboard[i] = i
        if (plugPairs) {
            plugPairs.split("-").each { pair ->
                int a = pair[0] - 'A', b = pair[1] - 'A'
                plugboard[a] = b; plugboard[b] = a
            }
        }
    }

    private int mod26(int n) { ((n % 26) + 26) % 26 }

    private void step() {
        if (offsets[1] == notches[1]) {
            offsets[1] = mod26(offsets[1] + 1)
            offsets[0] = mod26(offsets[0] + 1)
        } else if (offsets[2] == notches[2]) {
            offsets[1] = mod26(offsets[1] + 1)
        }
        offsets[2] = mod26(offsets[2] + 1)
    }

    private int fwdPass(int rotor, int idx) {
        int contact = mod26(idx + offsets[rotor])
        int out = FWD[rotors[rotor]][contact] - 'A'
        return mod26(out - offsets[rotor])
    }

    private int bwdPass(int rotor, int idx) {
        int contact = mod26(idx + offsets[rotor])
        int out = BWD[rotors[rotor]][contact] - 'A'
        return mod26(out - offsets[rotor])
    }

    char pressKey(char c) {
        step()
        int idx = plugboard[c - 'A']
        idx = fwdPass(2, idx)
        idx = fwdPass(1, idx)
        idx = fwdPass(0, idx)
        idx = REF[idx] - 'A'
        idx = bwdPass(0, idx)
        idx = bwdPass(1, idx)
        idx = bwdPass(2, idx)
        idx = plugboard[idx]
        return (char)('A' + idx)
    }

    String encrypt(String text) {
        text.toUpperCase().findAll { it ==~ /[A-Z]/ }.collect { pressKey(it as char) }.join()
    }
}

// Test harness
println "Enigma Machine - Groovy Implementation"
println "======================================="

def tests = [
    [[1,2,3], "AAA", "", "AAAAA", "BDZGO"],
    [[1,2,3], "AAA", "", "HELLOWORLD", "ILBDAAMTAZ"],
    [[1,2,3], "AAA", "", "ATTACKATDAWN", "BZHGNOCRRTCM"],
    [[1,2,3], "MCK", "", "HELLOWORLD", "DLTBBQVPQV"],
    [[3,1,2], "AAA", "", "HELLOWORLD", "KZHDFQYHXT"],
    [[1,2,3], "AAA", "AB-CD-EF", "HELLOWORLD", "IKACBBMTBF"]
]

int pass = 0
tests.eachWithIndex { t, i ->
    def (rotors, key, plug, input, expected) = t
    def e = new Enigma(rotors[0], rotors[1], rotors[2], key[0] as char, key[1] as char, key[2] as char, plug)
    def result = e.encrypt(input)
    def ok = result == expected
    if (ok) pass++
    println "Test ${i+1}: ${input} -> ${result} ${ok ? '[PASS]' : '[FAIL] expected ' + expected}"