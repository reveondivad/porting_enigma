' Enigma cipher in VBScript
Dim RF1, RF2, RF3, RB1, RB2, RB3, Ref, Notches, Pos
RF1 = Array(4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9)
RF2 = Array(0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4)
RF3 = Array(1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14)
RB1 = Array(20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9)
RB2 = Array(0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18)
RB3 = Array(19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12)
Ref = Array(24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19)
Notches = Array(16, 4, 21)
Pos = Array(0, 0, 0)

Function Mod26(n)
    Dim m : m = n Mod 26
    If m < 0 Then m = m + 26
    Mod26 = m
End Function

Function RotorPass(wiring, c, p)
    RotorPass = Mod26(wiring(Mod26(c + p)) - p)
End Function

Function Enigma(text)
    Pos(0) = 0 : Pos(1) = 0 : Pos(2) = 0
    Dim result, i, c, j, mid
    result = ""
    text = UCase(text)
    For i = 1 To Len(text)
        c = Asc(Mid(text, i, 1)) - 65
        If c >= 0 And c <= 25 Then
            mid = (Pos(1) = Notches(1))
            If Pos(2) = Notches(2) Then Pos(2) = Mod26(Pos(2) + 1)
            If mid Or Pos(2) = Notches(2) Then Pos(1) = Mod26(Pos(1) + 1)
            Pos(2) = Mod26(Pos(2) + 1)
            c = RotorPass(RF3, c, Pos(2))
            c = RotorPass(RF2, c, Pos(1))
            c = RotorPass(RF1, c, Pos(0))
            c = Ref(c)
            c = RotorPass(RB1, c, Pos(0))
            c = RotorPass(RB2, c, Pos(1))
            c = RotorPass(RB3, c, Pos(2))
            result = result & Chr(c + 65)
        End If
    Next
    Enigma = result
End Function

WScript.Echo Enigma("HELLOWORLD")
