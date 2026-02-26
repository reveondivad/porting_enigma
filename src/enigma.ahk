; Enigma Machine - AutoHotkey Implementation
; Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
; PeopleTec Inc. - Guinness World Record Attempt 2026

FWD := ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"]
BWD := ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"]
NOTCH := [16, 4, 21]
REFL := "YRUHQSLDPXNGOKMIEBFZCWVJAT"

Mod26(a) {
    return Mod(Mod(a, 26) + 26, 26)
}

C2I(c) {
    return Ord(c) - Ord("A")
}

I2C(i) {
    return Chr(i + Ord("A"))
}

class Enigma {
    __New(rotors, key, plugboard := "") {
        this.lFwd := FWD[rotors[1]], this.lBwd := BWD[rotors[1]], this.lNotch := NOTCH[rotors[1]]
        this.mFwd := FWD[rotors[2]], this.mBwd := BWD[rotors[2]], this.mNotch := NOTCH[rotors[2]]
        this.rFwd := FWD[rotors[3]], this.rBwd := BWD[rotors[3]], this.rNotch := NOTCH[rotors[3]]
        this.lOff := C2I(SubStr(key, 1, 1))
        this.mOff := C2I(SubStr(key, 2, 1))
        this.rOff := C2I(SubStr(key, 3, 1))
        this.plug := []
        Loop 26
            this.plug.Push(A_Index - 1)
        if (plugboard != "") {
            Loop Parse, plugboard, "-"
            {
                a := C2I(SubStr(A_LoopField, 1, 1))
                b := C2I(SubStr(A_LoopField, 2, 1))
                this.plug[a + 1] := b
                this.plug[b + 1] := a
            }
        }
    }

    FwdPass(wiring, off, idx) {
        contact := Mod26(idx + off)
        return Mod26(C2I(SubStr(wiring, contact + 1, 1)) - off)
    }

    BwdPass(wiring, off, idx) {
        contact := Mod26(idx + off)
        return Mod26(C2I(SubStr(wiring, contact + 1, 1)) - off)
    }

    StepRotors() {
        if (this.mOff = this.mNotch) {
            this.mOff := Mod(this.mOff + 1, 26)
            this.lOff := Mod(this.lOff + 1, 26)
        } else if (this.rOff = this.rNotch) {
            this.mOff := Mod(this.mOff + 1, 26)
        }
        this.rOff := Mod(this.rOff + 1, 26)
    }

    PressKey(c) {
        this.StepRotors()
        idx := C2I(c)
        idx := this.plug[idx + 1]
        idx := this.FwdPass(this.rFwd, this.rOff, idx)
        idx := this.FwdPass(this.mFwd, this.mOff, idx)
        idx := this.FwdPass(this.lFwd, this.lOff, idx)
        idx := C2I(SubStr(REFL, idx + 1, 1))
        idx := this.BwdPass(this.lBwd, this.lOff, idx)
        idx := this.BwdPass(this.mBwd, this.mOff, idx)
        idx := this.BwdPass(this.rBwd, this.rOff, idx)
        idx := this.plug[idx + 1]
        return I2C(idx)
    }

    Encrypt(text) {
        result := ""
        text := StrUpper(text)
        Loop Parse, text
        {
            if (Ord(A_LoopField) >= 65 && Ord(A_LoopField) <= 90)
                result .= this.PressKey(A_LoopField)
        }
        return result
    }
}

; Test harness
output := "Enigma Machine - AutoHotkey Implementation`n"
output .= "==========================================`n"

tests := [
    [[1,2,3], "AAA", "",       "AAAAA",        "BDZGO"],
    [[1,2,3], "AAA", "",       "HELLOWORLD",   "ILBDAAMTAZ"],
    [[1,2,3], "AAA", "",       "ATTACKATDAWN", "BZHGNOCRRTCM"],
    [[1,2,3], "MCK", "",       "HELLOWORLD",   "DLTBBQVPQV"],
    [[3,1,2], "AAA", "",       "HELLOWORLD",   "KZHDFQYHXT"],
    [[1,2,3], "AAA", "AB-CD-EF","HELLOWORLD", "IKACBBMTBF"]
]

allPass := true
for i, t in tests {
    e := Enigma(t[1], t[2], t[3])
    cipher := e.Encrypt(t[4])
    ok := (cipher = t[5])
    status := ok ? "PASS" : "FAIL"
    output .= "  Test " i ": " t[4] " -> " cipher " [" status "]`n"
    if (!ok) {
        output .= "          Expected " t[5] "`n"
        allPass := false
    }
}
output .= allPass ? "`n  ALL 6 TESTS PASSED" : "`n  SOME TESTS FAILED"
MsgBox(output)
