MODULE Enigma;
(* Enigma Machine - Modula-2 Implementation *)
(* Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping) *)
(* PeopleTec Inc. - Guinness World Record Attempt 2026 *)

FROM InOut IMPORT WriteString, WriteLn, WriteInt;

CONST
    FWD0 = "EKMFLGDQVZNTOWYHXUSPAIBRCJ";
    FWD1 = "AJDKSIRUXBLHWTMCQGZNPYFVOE";
    FWD2 = "BDFHJLCPRTXVZNYEIWGAKMUSQO";
    BWD0 = "UWYGADFPVZBECKMTHXSLRINQOJ";
    BWD1 = "AJPCZWRLFBDKOTYUQGENHXMIVS";
    BWD2 = "TAGBPCSDQEUFVNZHYIXJWLRKOM";
    REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT";
    NOTCH0 = 16; NOTCH1 = 4; NOTCH2 = 21;

TYPE
    Wiring = ARRAY [0..25] OF CHAR;

VAR
    lFwd, lBwd, mFwd, mBwd, rFwd, rBwd: Wiring;
    lOff, mOff, rOff: INTEGER;
    lNotch, mNotch, rNotch: INTEGER;
    plug: ARRAY [0..25] OF INTEGER;

PROCEDURE Mod26(a: INTEGER): INTEGER;
VAR r: INTEGER;
BEGIN
    r := a MOD 26;
    IF r < 0 THEN r := r + 26 END;
    RETURN r;
END Mod26;

PROCEDURE C2I(c: CHAR): INTEGER;
BEGIN RETURN ORD(c) - ORD('A'); END C2I;

PROCEDURE I2C(i: INTEGER): CHAR;
BEGIN RETURN CHR(i + ORD('A')); END I2C;

PROCEDURE CopyWiring(src: ARRAY OF CHAR; VAR dst: Wiring);
VAR i: INTEGER;
BEGIN
    FOR i := 0 TO 25 DO dst[i] := src[i]; END;
END CopyWiring;

PROCEDURE SetupRotor(num: INTEGER; VAR fwd, bwd: Wiring; VAR notch: INTEGER);
BEGIN
    CASE num OF
      0: CopyWiring(FWD0, fwd); CopyWiring(BWD0, bwd); notch := NOTCH0;
    | 1: CopyWiring(FWD1, fwd); CopyWiring(BWD1, bwd); notch := NOTCH1;
    | 2: CopyWiring(FWD2, fwd); CopyWiring(BWD2, bwd); notch := NOTCH2;
    END;
END SetupRotor;

PROCEDURE InitEnigma(r0, r1, r2: INTEGER; key: ARRAY OF CHAR;
                     pairs: ARRAY OF ARRAY OF CHAR; npairs: INTEGER);
VAR i, a, b: INTEGER;
BEGIN
    SetupRotor(r0, lFwd, lBwd, lNotch);
    SetupRotor(r1, mFwd, mBwd, mNotch);
    SetupRotor(r2, rFwd, rBwd, rNotch);
    lOff := C2I(key[0]); mOff := C2I(key[1]); rOff := C2I(key[2]);
    FOR i := 0 TO 25 DO plug[i] := i; END;
    FOR i := 0 TO npairs - 1 DO
        a := C2I(pairs[i][0]); b := C2I(pairs[i][1]);
        plug[a] := b; plug[b] := a;
    END;
END InitEnigma;

PROCEDURE FwdPass(VAR wiring: Wiring; off, idx: INTEGER): INTEGER;
VAR contact: INTEGER;
BEGIN
    contact := Mod26(idx + off);
    RETURN Mod26(C2I(wiring[contact]) - off);
END FwdPass;

PROCEDURE BwdPass(VAR wiring: Wiring; off, idx: INTEGER): INTEGER;
VAR contact: INTEGER;
BEGIN
    contact := Mod26(idx + off);
    RETURN Mod26(C2I(wiring[contact]) - off);
END BwdPass;

PROCEDURE StepRotors;
BEGIN
    IF mOff = mNotch THEN
        mOff := (mOff + 1) MOD 26;
        lOff := (lOff + 1) MOD 26;
    ELSIF rOff = rNotch THEN
        mOff := (mOff + 1) MOD 26;
    END;
    rOff := (rOff + 1) MOD 26;
END StepRotors;

PROCEDURE PressKey(c: CHAR): CHAR;
VAR idx: INTEGER; reflWiring: Wiring;
BEGIN
    StepRotors;
    CopyWiring(REFL, reflWiring);
    idx := C2I(c);
    idx := plug[idx];
    idx := FwdPass(rFwd, rOff, idx);
    idx := FwdPass(mFwd, mOff, idx);
    idx := FwdPass(lFwd, lOff, idx);
    idx := C2I(reflWiring[idx]);
    idx := BwdPass(lBwd, lOff, idx);
    idx := BwdPass(mBwd, mOff, idx);
    idx := BwdPass(rBwd, rOff, idx);
    idx := plug[idx];
    RETURN I2C(idx);
END PressKey;

PROCEDURE Encrypt(text: ARRAY OF CHAR; VAR result: ARRAY OF CHAR; VAR rlen: INTEGER);
VAR i: INTEGER; c: CHAR;
BEGIN
    rlen := 0;
    FOR i := 0 TO HIGH(text) DO
        c := CAP(text[i]);
        IF (c >= 'A') AND (c <= 'Z') THEN
            result[rlen] := PressKey(c);
            INC(rlen);
        END;
    END;
    result[rlen] := 0C;
END Encrypt;

PROCEDURE StrEq(a, b: ARRAY OF CHAR): BOOLEAN;
VAR i: INTEGER;
BEGIN
    i := 0;
    WHILE (i <= HIGH(a)) AND (i <= HIGH(b)) AND (a[i] # 0C) AND (b[i] # 0C) DO
        IF a[i] # b[i] THEN RETURN FALSE; END;
        INC(i);
    END;
    RETURN (a[i] = 0C) AND (b[i] = 0C);
END StrEq;

VAR
    cipher: ARRAY [0..63] OF CHAR;
    clen: INTEGER;
    allPass: BOOLEAN;
    noPairs: ARRAY [0..0] OF ARRAY [0..1] OF CHAR;
    plugPairs: ARRAY [0..2] OF ARRAY [0..1] OF CHAR;

BEGIN
    WriteString("Enigma Machine - Modula-2 Implementation"); WriteLn;
    WriteString("========================================="); WriteLn;
    allPass := TRUE;

    InitEnigma(0, 1, 2, "AAA", noPairs, 0);
    Encrypt("AAAAA", cipher, clen);
    WriteString("  Test 1: AAAAA        -> "); WriteString(cipher);
    IF StrEq(cipher, "BDZGO") THEN WriteString(" [PASS]");
    ELSE WriteString(" [FAIL]"); allPass := FALSE; END; WriteLn;

    InitEnigma(0, 1, 2, "AAA", noPairs, 0);
    Encrypt("HELLOWORLD", cipher, clen);
    WriteString("  Test 2: HELLOWORLD   -> "); WriteString(cipher);
    IF StrEq(cipher, "ILBDAAMTAZ") THEN WriteString(" [PASS]");
    ELSE WriteString(" [FAIL]"); allPass := FALSE; END; WriteLn;

    InitEnigma(0, 1, 2, "AAA", noPairs, 0);
    Encrypt("ATTACKATDAWN", cipher, clen);
    WriteString("  Test 3: ATTACKATDAWN -> "); WriteString(cipher);
    IF StrEq(cipher, "BZHGNOCRRTCM") THEN WriteString(" [PASS]");
    ELSE WriteString(" [FAIL]"); allPass := FALSE; END; WriteLn;

    InitEnigma(0, 1, 2, "MCK", noPairs, 0);
    Encrypt("HELLOWORLD", cipher, clen);
    WriteString("  Test 4: HELLOWORLD   -> "); WriteString(cipher);
    IF StrEq(cipher, "DLTBBQVPQV") THEN WriteString(" [PASS]");
    ELSE WriteString(" [FAIL]"); allPass := FALSE; END; WriteLn;

    InitEnigma(2, 0, 1, "AAA", noPairs, 0);
    Encrypt("HELLOWORLD", cipher, clen);
    WriteString("  Test 5: HELLOWORLD   -> "); WriteString(cipher);
    IF StrEq(cipher, "KZHDFQYHXT") THEN WriteString(" [PASS]");
    ELSE WriteString(" [FAIL]"); allPass := FALSE; END; WriteLn;

    plugPairs[0] := "AB"; plugPairs[1] := "CD"; plugPairs[2] := "EF";
    InitEnigma(0, 1, 2, "AAA", plugPairs, 3);
    Encrypt("HELLOWORLD", cipher, clen);
    WriteString("  Test 6: HELLOWORLD   -> "); WriteString(cipher);
    IF StrEq(cipher, "IKACBBMTBF") THEN WriteString(" [PASS]");
    ELSE WriteString(" [FAIL]"); allPass := FALSE; END; WriteLn;

    WriteLn;
    IF allPass THEN WriteString("  ALL 6 TESTS PASSED");
    ELSE WriteString("  SOME TESTS FAILED"); END;
    WriteLn;
END Enigma.
