Red [
    Title: "Enigma Machine"
    Description: "Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)"
    Author: "PeopleTec Inc. - Guinness World Record Attempt 2026"
]

fwd: ["EKMFLGDQVZNTOWYHXUSPAIBRCJ" "AJDKSIRUXBLHWTMCQGZNPYFVOE" "BDFHJLCPRTXVZNYEIWGAKMUSQO"]
bwd: ["UWYGADFPVZBECKMTHXSLRINQOJ" "AJPCZWRLFBDKOTYUQGENHXMIVS" "TAGBPCSDQEUFVNZHYIXJWLRKOM"]
notches: [17 5 22]  ; 1-based: Q=17, E=5, V=22
refl: "YRUHQSLDPXNGOKMIEBFZCWVJAT"

mod26: func [a][r: a % 26 if r < 0 [r: r + 26] r]
c2i: func [c][to-integer c - 65]
i2c: func [i][to-char i + 65]

make-rotor: func [num win][
    context [
        fwd-w: pick fwd num
        bwd-w: pick bwd num
        notch: pick notches num
        offset: c2i win

        forward: func [idx][
            contact: mod26 idx + offset
            mod26 (c2i pick fwd-w (contact + 1)) - offset
        ]
        backward: func [idx][
            contact: mod26 idx + offset
            mod26 (c2i pick bwd-w (contact + 1)) - offset
        ]
        step-r: does [offset: (offset + 1) % 26]
        at-notch: does [offset + 1 = notch]
    ]
]

make-enigma: func [rotors key plugboard][
    context [
        left: make-rotor pick rotors 1 pick key 1
        middle: make-rotor pick rotors 2 pick key 2
        right: make-rotor pick rotors 3 pick key 3
        plug: copy []
        repeat i 26 [append plug i - 1]

        if plugboard [
            foreach pair plugboard [
                a: c2i pick pair 1
                b: c2i pick pair 2
                poke plug (a + 1) b
                poke plug (b + 1) a
            ]
        ]

        step-rotors: does [
            either middle/at-notch [
                middle/step-r
                left/step-r
            ][
                if right/at-notch [middle/step-r]
            ]
            right/step-r
        ]

        press-key: func [c][
            step-rotors
            idx: c2i c
            idx: pick plug (idx + 1)
            idx: right/forward idx
            idx: middle/forward idx
            idx: left/forward idx
            idx: c2i pick refl (idx + 1)
            idx: left/backward idx
            idx: middle/backward idx
            idx: right/backward idx
            idx: pick plug (idx + 1)
            i2c idx
        ]

        encrypt: func [text][
            result: copy ""
            foreach c uppercase text [
                if all [c >= #"A" c <= #"Z"][
                    append result press-key c
                ]
            ]
            result
        ]
    ]
]

; Test harness
print "Enigma Machine - Red Implementation"
print "===================================="

tests: reduce [
    [1 2 3] "AAA" none        "AAAAA"        "BDZGO"
    [1 2 3] "AAA" none        "HELLOWORLD"   "ILBDAAMTAZ"
    [1 2 3] "AAA" none        "ATTACKATDAWN" "BZHGNOCRRTCM"
    [1 2 3] "MCK" none        "HELLOWORLD"   "DLTBBQVPQV"
    [3 1 2] "AAA" none        "HELLOWORLD"   "KZHDFQYHXT"
    [1 2 3] "AAA" ["AB" "CD" "EF"] "HELLOWORLD" "IKACBBMTBF"
]

all-pass: true
i: 1
forskip tests 5 [
    e: make-enigma tests/1 tests/2 tests/3
    cipher: e/encrypt tests/4
    ok: cipher = tests/5
    status: either ok ["PASS"]["FAIL"]
    print rejoin ["  Test " i ": " tests/4 " -> " cipher " [" status "]"]
    unless ok [
        print rejoin ["          Expected " tests/5]
        all-pass: false
    ]
    i: i + 1
]
print either all-pass ["^/  ALL 6 TESTS PASSED"]["^/  SOME TESTS FAILED"]
