// Enigma Cipher - CUE (Configure, Unify, Execute)
// Constraint-based configuration language from Google
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

package enigma

#Wiring: [26]int

#RotorI_Fwd: #Wiring & [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
#RotorII_Fwd: #Wiring & [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
#RotorIII_Fwd: #Wiring & [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]

#RotorI_Bwd: #Wiring & [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
#RotorII_Bwd: #Wiring & [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
#RotorIII_Bwd: #Wiring & [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]

#ReflectorB: #Wiring & [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
#Notches: [16, 4, 21]

// CUE is primarily a configuration/validation language.
// This defines the Enigma machine configuration schema and test vectors.

#EnigmaConfig: {
    rotors:  [3]int & [>=1, >=1, >=1] & [<=3, <=3, <=3]
    key:     [3]int & [>=0, >=0, >=0] & [<=25, <=25, <=25]
    plug:    string | *""
    message: string
}

#TestVector: {
    config:   #EnigmaConfig
    expected: string
}

tests: [string]: #TestVector
tests: {
    test1: {
        config: { rotors: [1,2,3], key: [0,0,0], message: "AAAAA" }
        expected: "BDZGO"
    }
    test2: {
        config: { rotors: [1,2,3], key: [0,0,0], message: "HELLOWORLD" }
        expected: "ILBDAAMTAZ"
    }
    test3: {
        config: { rotors: [1,2,3], key: [0,0,0], message: "ATTACKATDAWN" }
        expected: "BZHGNOCRRTCM"
    }
    test4: {
        config: { rotors: [1,2,3], key: [12,2,10], message: "HELLOWORLD" }
        expected: "DLTBBQVPQV"
    }
    test5: {
        config: { rotors: [3,1,2], key: [0,0,0], message: "HELLOWORLD" }
        expected: "KZHDFQYHXT"
    }
    test6: {
        config: { rotors: [1,2,3], key: [0,0,0], plug: "AB-CD-EF", message: "HELLOWORLD" }
        expected: "IKACBBMTBF"
    }
}

// Algorithm description in CUE constraint notation
description: "Wehrmacht Enigma I cipher with 3 rotors (I, II, III), Reflector B, plugboard, and double-stepping anomaly"
