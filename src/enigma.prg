// Enigma Cipher - Harbour
// xBase/Clipper descendant, database-oriented language

PROCEDURE Main()
    LOCAL aFwdI := {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9}
    LOCAL aFwdII := {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4}
    LOCAL aFwdIII := {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14}
    LOCAL aBwdI := {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9}
    LOCAL aBwdII := {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18}
    LOCAL aBwdIII := {19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10}
    LOCAL aNotch := {16, 4, 21}
    LOCAL aRefl := {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19}
    LOCAL aFwd := {aFwdI, aFwdII, aFwdIII}
    LOCAL aBwd := {aBwdI, aBwdII, aBwdIII}
    LOCAL aPB, cResult

    ? "Enigma Cipher - Harbour"

    // Test 1
    aPB := MakePB({})
    cResult := Encrypt(aFwd, aBwd, aNotch, aRefl, 1,2,3, 0,0,0, aPB, "AAAAA")
    RunTest("Test 1", "BDZGO", cResult)

    // Test 2
    aPB := MakePB({})
    cResult := Encrypt(aFwd, aBwd, aNotch, aRefl, 1,2,3, 0,0,0, aPB, "HELLOWORLD")
    RunTest("Test 2", "ILBDAAMTAZ", cResult)

    // Test 3
    aPB := MakePB({})
    cResult := Encrypt(aFwd, aBwd, aNotch, aRefl, 1,2,3, 0,0,0, aPB, "ATTACKATDAWN")
    RunTest("Test 3", "BZHGNOCRRTCM", cResult)

    // Test 4
    aPB := MakePB({})
    cResult := Encrypt(aFwd, aBwd, aNotch, aRefl, 1,2,3, 12,2,10, aPB, "HELLOWORLD")
    RunTest("Test 4", "DLTBBQVPQV", cResult)

    // Test 5
    aPB := MakePB({})
    cResult := Encrypt(aFwd, aBwd, aNotch, aRefl, 3,1,2, 0,0,0, aPB, "HELLOWORLD")
    RunTest("Test 5", "KZHDFQYHXT", cResult)

    // Test 6
    aPB := MakePB({{0,1},{2,3},{4,5}})
    cResult := Encrypt(aFwd, aBwd, aNotch, aRefl, 1,2,3, 0,0,0, aPB, "HELLOWORLD")
    RunTest("Test 6", "IKACBBMTBF", cResult)
RETURN

FUNCTION Mod26(x)
    LOCAL m := x % 26
    IF m < 0; m += 26; ENDIF
RETURN m

FUNCTION PassFwd(aFwd, nRotor, nOff, nCh)
    LOCAL inp := Mod26(nCh + nOff)
    LOCAL out := aFwd[nRotor][inp + 1]
RETURN Mod26(out - nOff)

FUNCTION PassBwd(aBwd, nRotor, nOff, nCh)
    LOCAL inp := Mod26(nCh + nOff)
    LOCAL out := aBwd[nRotor][inp + 1]
RETURN Mod26(out - nOff)

FUNCTION MakePB(aPairs)
    LOCAL aPB := Array(26)
    LOCAL i, p
    FOR i := 1 TO 26; aPB[i] := i - 1; NEXT
    FOR EACH p IN aPairs
        aPB[p[1] + 1] := p[2]
        aPB[p[2] + 1] := p[1]
    NEXT
RETURN aPB

FUNCTION Encrypt(aFwd, aBwd, aNotch, aRefl, nR0, nR1, nR2, nK0, nK1, nK2, aPB, cMsg)
    LOCAL o0 := nK0, o1 := nK1, o2 := nK2
    LOCAL cResult := "", i, ch, c, lMid, lAtn
    FOR i := 1 TO Len(cMsg)
        ch := Asc(SubStr(cMsg, i, 1)) - Asc("A")
        lMid := (o1 == aNotch[nR1])
        lAtn := (o2 == aNotch[nR2])
        o2 := Mod26(o2 + 1)
        IF lAtn .OR. lMid; o1 := Mod26(o1 + 1); ENDIF
        IF lMid; o0 := Mod26(o0 + 1); ENDIF
        c := aPB[ch + 1]
        c := PassFwd(aFwd, nR2, o2, c)
        c := PassFwd(aFwd, nR1, o1, c)
        c := PassFwd(aFwd, nR0, o0, c)
        c := aRefl[c + 1]
        c := PassBwd(aBwd, nR0, o0, c)
        c := PassBwd(aBwd, nR1, o1, c)
        c := PassBwd(aBwd, nR2, o2, c)
        c := aPB[c + 1]
        cResult += Chr(c + Asc("A"))
    NEXT
RETURN cResult

PROCEDURE RunTest(cLabel, cExpected, cActual)
    LOCAL cStatus
    IF cExpected == cActual; cStatus := "PASS"; ELSE; cStatus := "FAIL"; ENDIF
    ? cStatus + " " + cLabel + ": " + cActual + " (expected " + cExpected + ")"
RETURN
