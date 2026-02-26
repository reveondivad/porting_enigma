' Enigma Machine - Visual Basic .NET Implementation
' Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
' PeopleTec Inc. - Guinness World Record Attempt 2026

Module EnigmaModule
    Dim FWD() As String = {"EKMFLGDQVZNTOWYHXUSPAIBRCJ", _
                            "AJDKSIRUXBLHWTMCQGZNPYFVOE", _
                            "BDFHJLCPRTXVZNYEIWGAKMUSQO"}
    Dim BWD() As String = {"UWYGADFPVZBECKMTHXSLRINQOJ", _
                            "AJPCZWRLFBDKOTYUQGENHXMIVS", _
                            "TAGBPCSDQEUFVNZHYIXJWLRKOM"}
    Dim NOTCH_POS() As Integer = {16, 4, 21}
    Dim REFL As String = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

    Function Mod26(a As Integer) As Integer
        Return ((a Mod 26) + 26) Mod 26
    End Function

    Class Rotor
        Public fwd As String, bwd As String
        Public notch As Integer, offset As Integer

        Sub New(num As Integer, win As Char)
            fwd = FWD(num) : bwd = BWD(num)
            notch = NOTCH_POS(num)
            offset = Asc(win) - Asc("A"c)
        End Sub

        Function ForwardPass(idx As Integer) As Integer
            Dim contact As Integer = Mod26(idx + offset)
            Return Mod26(Asc(fwd(contact)) - Asc("A"c) - offset)
        End Function

        Function BackwardPass(idx As Integer) As Integer
            Dim contact As Integer = Mod26(idx + offset)
            Return Mod26(Asc(bwd(contact)) - Asc("A"c) - offset)
        End Function

        Sub StepRotor()
            offset = (offset + 1) Mod 26
        End Sub

        Function AtNotch() As Boolean
            Return offset = notch
        End Function
    End Class

    Class Enigma
        Public left, middle, right As Rotor
        Public plug(25) As Integer

        Sub New(rotors() As Integer, key As String, plugboard() As String)
            left = New Rotor(rotors(0), key(0))
            middle = New Rotor(rotors(1), key(1))
            right = New Rotor(rotors(2), key(2))
            For i As Integer = 0 To 25 : plug(i) = i : Next
            If plugboard IsNot Nothing Then
                For Each pair As String In plugboard
                    Dim a As Integer = Asc(pair(0)) - Asc("A"c)
                    Dim b As Integer = Asc(pair(1)) - Asc("A"c)
                    plug(a) = b : plug(b) = a
                Next
            End If
        End Sub

        Sub StepRotors()
            If middle.AtNotch() Then
                middle.StepRotor() : left.StepRotor()
            ElseIf right.AtNotch() Then
                middle.StepRotor()
            End If
            right.StepRotor()
        End Sub

        Function PressKey(c As Char) As Char
            StepRotors()
            Dim idx As Integer = Asc(c) - Asc("A"c)
            idx = plug(idx)
            idx = right.ForwardPass(idx)
            idx = middle.ForwardPass(idx)
            idx = left.ForwardPass(idx)
            idx = Asc(REFL(idx)) - Asc("A"c)
            idx = left.BackwardPass(idx)
            idx = middle.BackwardPass(idx)
            idx = right.BackwardPass(idx)
            idx = plug(idx)
            Return Chr(idx + Asc("A"c))
        End Function

        Function Encrypt(text As String) As String
            Dim result As String = ""
            For Each c As Char In text.ToUpper()
                If c >= "A"c AndAlso c <= "Z"c Then result &= PressKey(c)
            Next
            Return result
        End Function
    End Class

    Sub Main()
        Console.WriteLine("Enigma Machine - VB.NET Implementation")
        Console.WriteLine("=======================================")

        Dim tests = {
            ({0,1,2}, "AAA", Nothing, "AAAAA", "BDZGO"),
            ({0,1,2}, "AAA", Nothing, "HELLOWORLD", "ILBDAAMTAZ"),
            ({0,1,2}, "AAA", Nothing, "ATTACKATDAWN", "BZHGNOCRRTCM"),
            ({0,1,2}, "MCK", Nothing, "HELLOWORLD", "DLTBBQVPQV"),
            ({2,0,1}, "AAA", Nothing, "HELLOWORLD", "KZHDFQYHXT"),
            ({0,1,2}, "AAA", New String(){"AB","CD","EF"}, "HELLOWORLD", "IKACBBMTBF")
        }

        Dim allPass As Boolean = True
        For i As Integer = 0 To tests.Length - 1
            Dim t = tests(i)
            Dim e As New Enigma(t.Item1, t.Item2, t.Item3)
            Dim cipher As String = e.Encrypt(t.Item4)
            Dim ok As Boolean = cipher = t.Item5
            Dim status As String = If(ok, "PASS", "FAIL")
            Console.WriteLine($"  Test {i+1}: {t.Item4,-20} -> {cipher,-15} [{status}]")
            If Not ok Then Console.WriteLine($"          Expected {t.Item5}") : allPass = False
        Next
        Console.WriteLine(If(allPass, vbLf & "  ALL 6 TESTS PASSED", vbLf & "  SOME TESTS FAILED"))
    End Sub
End Module
