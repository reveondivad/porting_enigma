/* Enigma Cipher - YARA
   Pattern matching language for malware research
   Wehrmacht Enigma I config and test vectors as YARA rule
   PeopleTec Inc. - Guinness World Record Attempt 2026 */

import "math"

rule Enigma_Cipher_Implementation {
    meta:
        description = "Detects Enigma cipher implementation patterns"
        author = "PeopleTec Inc."
        purpose = "Guinness World Record - 500 language Enigma"

    strings:
        // Rotor I forward wiring: EKMFLGDQVZNTOWYHXUSPAIBRCJ
        $rotor_i_alpha = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
        $rotor_i_nums = { 04 0A 0C 05 0B 06 03 10 15 19 0D 13 0E 16 18 07 17 14 12 0F 00 08 01 11 02 09 }

        // Rotor II forward wiring: AJDKSIRUXBLHWTMCQGZNPYFVOE
        $rotor_ii_alpha = "AJDKSIRUXBLHWTMCQGZNPYFVOE"

        // Rotor III forward wiring: BDFHJLCPRTXVZNYEIWGAKMUSQO
        $rotor_iii_alpha = "BDFHJLCPRTXVZNYEIWGAKMUSQO"

        // Reflector B: YRUHQSLDPXNGOKMIEBFZCWVJAT
        $reflector_b = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

        // Test vectors
        $test_aaaaa = "BDZGO"
        $test_hello = "ILBDAAMTAZ"
        $test_attack = "BZHGNOCRRTCM"

        // Common code patterns
        $mod26_pattern = /mod\s*26|%\s*26|\bmod26\b/
        $double_step = /double.?step|notch/

    condition:
        any of ($rotor_*) and
        $reflector_b and
        any of ($test_*) and
        $mod26_pattern
}

rule Enigma_Test_Vectors {
    meta:
        description = "Known Enigma test vector outputs"
        rotors = "I-II-III"
        reflector = "B"

    strings:
        $t1 = "BDZGO"        // AAAAA with key AAA
        $t2 = "ILBDAAMTAZ"   // HELLOWORLD with key AAA
        $t3 = "BZHGNOCRRTCM" // ATTACKATDAWN with key AAA
        $t4 = "DLTBBQVPQV"   // HELLOWORLD with key MCK
        $t5 = "KZHDFQYHXT"   // HELLOWORLD rotors III-I-II key AAA

    condition:
        3 of them
}
