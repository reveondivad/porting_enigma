"Enigma Machine - GNU Smalltalk Implementation"
"Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)"
"PeopleTec Inc. - Guinness World Record Attempt 2026"

Object subclass: #Enigma
    instanceVariableNames: 'rotors offsets notches plugboard'
    classVariableNames: 'FWD BWD REF NOTCH'
    poolDictionaries: ''
    category: 'Enigma'.

Enigma class >> initialize [
    FWD := #('EKMFLGDQVZNTOWYHXUSPAIBRCJ' 'AJDKSIRUXBLHWTMCQGZNPYFVOE' 'BDFHJLCPRTXVZNYEIWGAKMUSQO').
    BWD := #('UWYGADFPVZBECKMTHXSLRINQOJ' 'AJPCZWRLFBDKOTYUQGENHXMIVS' 'TAGBPCSDQEUFVNZHYIXJWLRKOM').
    REF := 'YRUHQSLDPXNGOKMIEBFZCWVJAT'.
    NOTCH := #(16 4 21).
]

Enigma class >> r1: r1 r2: r2 r3: r3 k1: k1 k2: k2 k3: k3 plug: plugPairs [
    | e ri |
    self initialize.
    e := self new.
    ri := Array with: r1 - 1 with: r2 - 1 with: r3 - 1.
    e init: ri k1: k1 k2: k2 k3: k3 plug: plugPairs.
    ^ e
]

Enigma >> init: ri k1: k1 k2: k2 k3: k3 plug: plugPairs [
    rotors := ri.
    offsets := Array with: k1 asciiValue - 65 with: k2 asciiValue - 65 with: k3 asciiValue - 65.
    notches := Array with: (NOTCH at: (ri at: 1) + 1) with: (NOTCH at: (ri at: 2) + 1) with: (NOTCH at: (ri at: 3) + 1).
    plugboard := (0 to: 25) asArray.
    plugPairs isEmpty ifFalse: [
        (plugPairs subStrings: $-) do: [:pair |
            | a b |
            a := (pair at: 1) asciiValue - 65.
            b := (pair at: 2) asciiValue - 65.
            plugboard at: a + 1 put: b.
            plugboard at: b + 1 put: a.
        ].
    ].
]

Enigma >> mod26: n [
    ^ ((n \\ 26) + 26) \\ 26
]

Enigma >> step [
    (offsets at: 2) = (notches at: 2) ifTrue: [
        offsets at: 2 put: (self mod26: (offsets at: 2) + 1).
        offsets at: 1 put: (self mod26: (offsets at: 1) + 1).
    ] ifFalse: [
        (offsets at: 3) = (notches at: 3) ifTrue: [
            offsets at: 2 put: (self mod26: (offsets at: 2) + 1).
        ].
    ].
    offsets at: 3 put: (self mod26: (offsets at: 3) + 1).
]

Enigma >> fwdPass: rotor idx: idx [
    | contact out |
    contact := self mod26: idx + (offsets at: rotor).
    out := ((FWD at: (rotors at: rotor) + 1) at: contact + 1) asciiValue - 65.
    ^ self mod26: out - (offsets at: rotor)
]

Enigma >> bwdPass: rotor idx: idx [
    | contact out |
    contact := self mod26: idx + (offsets at: rotor).
    out := ((BWD at: (rotors at: rotor) + 1) at: contact + 1) asciiValue - 65.
    ^ self mod26: out - (offsets at: rotor)
]

Enigma >> pressKey: c [
    | idx |
    self step.
    idx := plugboard at: (c asciiValue - 65) + 1.
    idx := self fwdPass: 3 idx: idx.
    idx := self fwdPass: 2 idx: idx.
    idx := self fwdPass: 1 idx: idx.
    idx := (REF at: idx + 1) asciiValue - 65.
    idx := self bwdPass: 1 idx: idx.
    idx := self bwdPass: 2 idx: idx.
    idx := self bwdPass: 3 idx: idx.
    idx := plugboard at: idx + 1.
    ^ (Character value: 65 + idx)
]

Enigma >> encrypt: text [
    | result upper |
    result := WriteStream on: String new.
    upper := text asUppercase.
    upper do: [:c |
        (c isLetter and: [c isUppercase]) ifTrue: [
            result nextPut: (self pressKey: c).
        ].
    ].
    ^ result contents
]

| tests pass |
Transcript show: 'Enigma Machine - GNU Smalltalk Implementation'; cr.
Transcript show: '================================================'; cr.

tests := {
    {#(1 2 3). 'AAA'. ''. 'AAAAA'. 'BDZGO'}.
    {#(1 2 3). 'AAA'. ''. 'HELLOWORLD'. 'ILBDAAMTAZ'}.
    {#(1 2 3). 'AAA'. ''. 'ATTACKATDAWN'. 'BZHGNOCRRTCM'}.
    {#(1 2 3). 'MCK'. ''. 'HELLOWORLD'. 'DLTBBQVPQV'}.
    {#(3 1 2). 'AAA'. ''. 'HELLOWORLD'. 'KZHDFQYHXT'}.
    {#(1 2 3). 'AAA'. 'AB-CD-EF'. 'HELLOWORLD'. 'IKACBBMTBF'}.
}.

pass := 0.
1 to: tests size do: [:i |
    | t r k e result ok |
    t := tests at: i.
    r := t at: 1. k := t at: 2.
    e := Enigma r1: (r at: 1) r2: (r at: 2) r3: (r at: 3)
               k1: (k at: 1) k2: (k at: 2) k3: (k at: 3) plug: (t at: 3).
    result := e encrypt: (t at: 4).
    ok := result = (t at: 5).
    ok ifTrue: [pass := pass + 1].
    Transcript show: 'Test ', i printString, ': ', (t at: 4), ' -> ', result, ' ',
        (ok ifTrue: ['[PASS]'] ifFalse: ['[FAIL] expected ', (t at: 5)]); cr.
].
Transcript show: pass printString, '/6 tests passed'; cr.