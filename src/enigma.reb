REBOL [
    Title: "Enigma Cipher"
    Purpose: "Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)"
    Author: "PeopleTec Inc."
    Date: 2026-02-23
]

fwd: [
    [4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9]
    [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4]
    [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14]
]
bwd: [
    [20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9]
    [0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18]
    [19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12]
]
reflector: [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19]
notches: [16 4 21]

mod26: func [n [integer!]] [
    r: remainder n 26
    if r < 0 [r: r + 26]
    r
]

context [
    rotors: [0 0 0]
    offsets: [0 0 0]
    rnotch: [0 0 0]
    pb: []

    set 'init-enigma func [r1 r2 r3 k1 k2 k3] [
        rotors: reduce [r1 - 1 r2 - 1 r3 - 1]
        offsets: reduce [k1 k2 k3]
        rnotch: reduce [
            pick notches rotors/1 + 1
            pick notches rotors/2 + 1
            pick notches rotors/3 + 1
        ]
        pb: copy []
        repeat i 26 [append pb i - 1]
    ]

    set 'step-rotors does [
        either offsets/2 = rnotch/2 [
            offsets/2: mod26 offsets/2 + 1
            offsets/1: mod26 offsets/1 + 1
        ][
            if offsets/3 = rnotch/3 [
                offsets/2: mod26 offsets/2 + 1
            ]
        ]
        offsets/3: mod26 offsets/3 + 1
    ]

    set 'fwd-pass func [rotor idx] [
        contact: mod26 idx + pick offsets rotor + 1
        wiring: pick fwd pick rotors rotor + 1 + 1
        out: pick wiring contact + 1
        mod26 out - (pick offsets rotor + 1)
    ]

    set 'bwd-pass func [rotor idx] [
        contact: mod26 idx + pick offsets rotor + 1
        wiring: pick bwd pick rotors rotor + 1 + 1
        out: pick wiring contact + 1
        mod26 out - (pick offsets rotor + 1)
    ]

    set 'press-key func [ch] [
        step-rotors
        idx: pick pb ch + 1
        idx: fwd-pass 2 idx
        idx: fwd-pass 1 idx
        idx: fwd-pass 0 idx
        idx: pick reflector idx + 1
        idx: bwd-pass 0 idx
        idx: bwd-pass 1 idx
        idx: bwd-pass 2 idx
        pick pb idx + 1
    ]

    set 'encrypt func [text [string!]] [
        result: copy ""
        foreach ch uppercase text [
            if all [ch >= #"A" ch <= #"Z"] [
                enc: press-key (to-integer ch) - 65
                append result to-char enc + 65
            ]
        ]
        result
    ]
]

print "Enigma Cipher - REBOL"
init-enigma 1 2 3 0 0 0
print ["Test 1:" encrypt "AAAAA" "expected BDZGO"]
init-enigma 1 2 3 0 0 0
print ["Test 2:" encrypt "HELLOWORLD" "expected ILBDAAMTAZ"]
init-enigma 1 2 3 0 0 0
print ["Test 3:" encrypt "ATTACKATDAWN" "expected BZHGNOCRRTCM"]
init-enigma 1 2 3 12 2 10
print ["Test 4:" encrypt "HELLOWORLD" "expected DLTBBQVPQV"]
init-enigma 3 1 2 0 0 0
print ["Test 5:" encrypt "HELLOWORLD" "expected KZHDFQYHXT"]
