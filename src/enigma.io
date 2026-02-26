# Enigma Machine - Io Implementation
# Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
# PeopleTec Inc. - Guinness World Record Attempt 2026

FWD := list("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO")
BWD := list("UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM")
NOTCH := list(16, 4, 21)
REFL := "YRUHQSLDPXNGOKMIEBFZCWVJAT"

mod26 := method(a, ((a % 26) + 26) % 26)
c2i := method(c, c at(0) - "A" at(0))
i2c := method(i, (i + "A" at(0)) asCharacter)

Rotor := Object clone do(
    fwd := nil; bwd := nil; notch := nil; offset := nil
    init := method(num, win,
        self fwd := FWD at(num)
        self bwd := BWD at(num)
        self notch := NOTCH at(num)
        self offset := c2i(win)
        self
    )
    forward := method(idx,
        contact := mod26(idx + offset)
        mod26(fwd at(contact) - "A" at(0) - offset)
    )
    backward := method(idx,
        contact := mod26(idx + offset)
        mod26(bwd at(contact) - "A" at(0) - offset)
    )
    step := method(self offset = (offset + 1) % 26)
    atNotch := method(offset == notch)
)

Enigma := Object clone do(
    left := nil; middle := nil; right := nil; plug := nil

    init := method(rotors, key, plugboard,
        self left := Rotor clone init(rotors at(0), key at(0) asCharacter)
        self middle := Rotor clone init(rotors at(1), key at(1) asCharacter)
        self right := Rotor clone init(rotors at(2), key at(2) asCharacter)
        self plug := list()
        26 repeat(i, plug append(i))
        if(plugboard,
            plugboard foreach(pair,
                a := c2i(pair at(0) asCharacter)
                b := c2i(pair at(1) asCharacter)
                plug atPut(a, b)
                plug atPut(b, a)
            )
        )
        self
    )

    stepRotors := method(
        if(middle atNotch,
            middle step
            left step
        ,
            if(right atNotch, middle step)
        )
        right step
    )

    pressKey := method(c,
        stepRotors
        idx := c at(0) - "A" at(0)
        idx = plug at(idx)
        idx = right forward(idx)
        idx = middle forward(idx)
        idx = left forward(idx)
        idx = REFL at(idx) - "A" at(0)
        idx = left backward(idx)
        idx = middle backward(idx)
        idx = right backward(idx)
        idx = plug at(idx)
        i2c(idx)
    )

    encrypt := method(text,
        result := ""
        text asUppercase foreach(i, c,
            ch := c asCharacter
            if(c >= "A" at(0) and c <= "Z" at(0),
                result = result .. pressKey(ch)
            )
        )
        result
    )
)

"Enigma Machine - Io Implementation" println
"=====================================" println

tests := list(
    list(list(0,1,2), "AAA", nil,                    "AAAAA",        "BDZGO"),
    list(list(0,1,2), "AAA", nil,                    "HELLOWORLD",   "ILBDAAMTAZ"),
    list(list(0,1,2), "AAA", nil,                    "ATTACKATDAWN", "BZHGNOCRRTCM"),
    list(list(0,1,2), "MCK", nil,                    "HELLOWORLD",   "DLTBBQVPQV"),
    list(list(2,0,1), "AAA", nil,                    "HELLOWORLD",   "KZHDFQYHXT"),
    list(list(0,1,2), "AAA", list("AB","CD","EF"),   "HELLOWORLD",   "IKACBBMTBF")
)

allPass := true
tests foreach(i, t,
    e := Enigma clone init(t at(0), t at(1), t at(2))
    cipher := e encrypt(t at(3))
    ok := cipher == t at(4)
    status := if(ok, "PASS", "FAIL")
    ("  Test " .. (i+1) .. ": " .. t at(3) .. " -> " .. cipher .. " [" .. status .. "]") println
    if(ok not,
        ("          Expected " .. t at(4)) println
        allPass = false
    )
)
if(allPass, "\n  ALL 6 TESTS PASSED" println, "\n  SOME TESTS FAILED" println)
