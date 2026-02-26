' Enigma Machine - FreeBASIC Implementation
' Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
' PeopleTec Inc. - Guinness World Record Attempt 2026

Dim Shared FWD(2) As String
Dim Shared BWD(2) As String
Dim Shared NOTCH_POS(2) As Integer
Dim Shared REFL As String

FWD(0) = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
FWD(1) = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
FWD(2) = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
BWD(0) = "UWYGADFPVZBECKMTHXSLRINQOJ"
BWD(1) = "AJPCZWRLFBDKOTYUQGENHXMIVS"
BWD(2) = "TAGBPCSDQEUFVNZHYIXJWLRKOM"
NOTCH_POS(0) = 16 : NOTCH_POS(1) = 4 : NOTCH_POS(2) = 21
REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

Function Mod26(a As Integer) As Integer
    Return ((a Mod 26) + 26) Mod 26
End Function

Type Rotor
    fwd_w As String
    bwd_w As String
    notch As Integer
    offset As Integer
End Type

Sub InitRotor(r As Rotor, num As Integer, win As String)
    r.fwd_w = FWD(num)
    r.bwd_w = BWD(num)
    r.notch = NOTCH_POS(num)
    r.offset = Asc(win) - Asc("A")
End Sub

Function FwdPass(r As Rotor, idx As Integer) As Integer
    Dim contact As Integer = Mod26(idx + r.offset)
    Return Mod26(Asc(Mid(r.fwd_w, contact + 1, 1)) - Asc("A") - r.offset)
End Function

Function BwdPass(r As Rotor, idx As Integer) As Integer
    Dim contact As Integer = Mod26(idx + r.offset)
    Return Mod26(Asc(Mid(r.bwd_w, contact + 1, 1)) - Asc("A") - r.offset)
End Function

Sub StepRotor(r As Rotor)
    r.offset = (r.offset + 1) Mod 26
End Sub

Function AtNotch(r As Rotor) As Integer
    Return (r.offset = r.notch)
End Function

Type Enigma
    left As Rotor
    middle As Rotor
    right As Rotor
    plug(25) As Integer
End Type

Sub InitEnigma(e As Enigma, r1 As Integer, r2 As Integer, r3 As Integer, key As String, plugboard As String)
    InitRotor(e.left, r1, Mid(key, 1, 1))
    InitRotor(e.middle, r2, Mid(key, 2, 1))
    InitRotor(e.right, r3, Mid(key, 3, 1))
    For i As Integer = 0 To 25
        e.plug(i) = i
    Next
    If Len(plugboard) > 0 Then
        Dim pairs() As String
        Dim n As Integer = 0
        Dim p As Integer = 1
        Do While p <= Len(plugboard)
            Dim sep As Integer = InStr(p, plugboard, "-")
            If sep = 0 Then sep = Len(plugboard) + 1
            Dim pair As String = Mid(plugboard, p, sep - p)
            If Len(pair) = 2 Then
                Dim a As Integer = Asc(Mid(pair, 1, 1)) - Asc("A")
                Dim b As Integer = Asc(Mid(pair, 2, 1)) - Asc("A")
                e.plug(a) = b : e.plug(b) = a
            End If
            p = sep + 1
        Loop
    End If
End Sub

Sub StepRotors(e As Enigma)
    If AtNotch(e.middle) Then
        StepRotor(e.middle) : StepRotor(e.left)
    ElseIf AtNotch(e.right) Then
        StepRotor(e.middle)
    End If
    StepRotor(e.right)
End Sub

Function PressKey(e As Enigma, c As String) As String
    StepRotors(e)
    Dim idx As Integer = Asc(c) - Asc("A")
    idx = e.plug(idx)
    idx = FwdPass(e.right, idx)
    idx = FwdPass(e.middle, idx)
    idx = FwdPass(e.left, idx)
    idx = Asc(Mid(REFL, idx + 1, 1)) - Asc("A")
    idx = BwdPass(e.left, idx)
    idx = BwdPass(e.middle, idx)
    idx = BwdPass(e.right, idx)
    idx = e.plug(idx)
    Return Chr(idx + Asc("A"))
End Function

Function Encrypt(e As Enigma, text As String) As String
    Dim result As String = ""
    Dim upper_text As String = UCase(text)
    For i As Integer = 1 To Len(upper_text)
        Dim c As String = Mid(upper_text, i, 1)
        If c >= "A" And c <= "Z" Then result = result + PressKey(e, c)
    Next
    Return result
End Function

' Test harness
Print "Enigma Machine - FreeBASIC Implementation"
Print "=========================================="

Dim As Integer allPass = -1
Dim As Enigma e

InitEnigma(e, 0, 1, 2, "AAA", "")
Dim cipher As String = Encrypt(e, "AAAAA")
Dim ok As Integer = (cipher = "BDZGO")
Print "  Test 1: AAAAA        -> "; cipher; " ["; IIf(ok, "PASS", "FAIL"); "]"
If Not ok Then Print "          Expected BDZGO" : allPass = 0

InitEnigma(e, 0, 1, 2, "AAA", "")
cipher = Encrypt(e, "HELLOWORLD")
ok = (cipher = "ILBDAAMTAZ")
Print "  Test 2: HELLOWORLD   -> "; cipher; " ["; IIf(ok, "PASS", "FAIL"); "]"
If Not ok Then Print "          Expected ILBDAAMTAZ" : allPass = 0

InitEnigma(e, 0, 1, 2, "AAA", "")
cipher = Encrypt(e, "ATTACKATDAWN")
ok = (cipher = "BZHGNOCRRTCM")
Print "  Test 3: ATTACKATDAWN -> "; cipher; " ["; IIf(ok, "PASS", "FAIL"); "]"
If Not ok Then Print "          Expected BZHGNOCRRTCM" : allPass = 0

InitEnigma(e, 0, 1, 2, "MCK", "")
cipher = Encrypt(e, "HELLOWORLD")
ok = (cipher = "DLTBBQVPQV")
Print "  Test 4: HELLOWORLD   -> "; cipher; " ["; IIf(ok, "PASS", "FAIL"); "]"
If Not ok Then Print "          Expected DLTBBQVPQV" : allPass = 0

InitEnigma(e, 2, 0, 1, "AAA", "")
cipher = Encrypt(e, "HELLOWORLD")
ok = (cipher = "KZHDFQYHXT")
Print "  Test 5: HELLOWORLD   -> "; cipher; " ["; IIf(ok, "PASS", "FAIL"); "]"
If Not ok Then Print "          Expected KZHDFQYHXT" : allPass = 0

InitEnigma(e, 0, 1, 2, "AAA", "AB-CD-EF")
cipher = Encrypt(e, "HELLOWORLD")
ok = (cipher = "IKACBBMTBF")
Print "  Test 6: HELLOWORLD   -> "; cipher; " ["; IIf(ok, "PASS", "FAIL"); "]"
If Not ok Then Print "          Expected IKACBBMTBF" : allPass = 0

If allPass Then Print : Print "  ALL 6 TESTS PASSED"
Else Print : Print "  SOME TESTS FAILED"
End If
