; Enigma Cipher - AutoIt
; Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
; PeopleTec Inc. - Guinness World Record Attempt 2026

Global $aFwdI[26]  = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
Global $aFwdII[26] = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
Global $aFwdIII[26]= [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
Global $aBwdI[26]  = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
Global $aBwdII[26] = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
Global $aBwdIII[26]= [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
Global $aRef[26]   = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
Global $aNotch[3]  = [16, 4, 21]

Global $gR[3], $gO[3], $gN[3], $gPB[26]

Func _Mod26($n)
    Local $m = Mod($n, 26)
    If $m < 0 Then $m += 26
    Return $m
EndFunc

Func _GetFwd($r, $i)
    Switch $r
        Case 0 : Return $aFwdI[$i]
        Case 1 : Return $aFwdII[$i]
        Case Else : Return $aFwdIII[$i]
    EndSwitch
EndFunc

Func _GetBwd($r, $i)
    Switch $r
        Case 0 : Return $aBwdI[$i]
        Case 1 : Return $aBwdII[$i]
        Case Else : Return $aBwdIII[$i]
    EndSwitch
EndFunc

Func _PassFwd($rotor, $offset, $ch)
    Local $inp = _Mod26($ch + $offset)
    Local $out = _GetFwd($rotor, $inp)
    Return _Mod26($out - $offset)
EndFunc

Func _PassBwd($rotor, $offset, $ch)
    Local $inp = _Mod26($ch + $offset)
    Local $out = _GetBwd($rotor, $inp)
    Return _Mod26($out - $offset)
EndFunc

Func _InitEnigma($r1, $r2, $r3, $k1, $k2, $k3)
    $gR[0] = $r1-1 : $gR[1] = $r2-1 : $gR[2] = $r3-1
    $gO[0] = $k1 : $gO[1] = $k2 : $gO[2] = $k3
    For $i = 0 To 2
        $gN[$i] = $aNotch[$gR[$i]]
    Next
    For $i = 0 To 25
        $gPB[$i] = $i
    Next
EndFunc

Func _StepRotors()
    If $gO[1] = $gN[1] Then
        $gO[1] = _Mod26($gO[1] + 1)
        $gO[0] = _Mod26($gO[0] + 1)
    ElseIf $gO[2] = $gN[2] Then
        $gO[1] = _Mod26($gO[1] + 1)
    EndIf
    $gO[2] = _Mod26($gO[2] + 1)
EndFunc

Func _PressKey($ch)
    _StepRotors()
    Local $c = $gPB[$ch]
    $c = _PassFwd($gR[2], $gO[2], $c)
    $c = _PassFwd($gR[1], $gO[1], $c)
    $c = _PassFwd($gR[0], $gO[0], $c)
    $c = $aRef[$c]
    $c = _PassBwd($gR[0], $gO[0], $c)
    $c = _PassBwd($gR[1], $gO[1], $c)
    $c = _PassBwd($gR[2], $gO[2], $c)
    Return $gPB[$c]
EndFunc

Func _Encrypt($sText)
    Local $sResult = ""
    $sText = StringUpper($sText)
    For $i = 1 To StringLen($sText)
        Local $ch = Asc(StringMid($sText, $i, 1)) - 65
        If $ch >= 0 And $ch < 26 Then
            $sResult &= Chr(_PressKey($ch) + 65)
        EndIf
    Next
    Return $sResult
EndFunc

ConsoleWrite("Enigma Cipher - AutoIt" & @CRLF)
Local $aTests[6][5] = [ _
    [1,2,3,"AAA","AAAAA"], [1,2,3,"AAA","HELLOWORLD"], _
    [1,2,3,"AAA","ATTACKATDAWN"], [1,2,3,"MCK","HELLOWORLD"], _
    [3,1,2,"AAA","HELLOWORLD"], [1,2,3,"AAA","HELLOWORLD"]]
Local $aExp[6] = ["BDZGO","ILBDAAMTAZ","BZHGNOCRRTCM","DLTBBQVPQV","KZHDFQYHXT","IKACBBMTBF"]

For $t = 0 To 5
    _InitEnigma($aTests[$t][0],$aTests[$t][1],$aTests[$t][2], _
                Asc(StringMid($aTests[$t][3],1,1))-65, _
                Asc(StringMid($aTests[$t][3],2,1))-65, _
                Asc(StringMid($aTests[$t][3],3,1))-65)
    Local $res = _Encrypt($aTests[$t][4])
    Local $ok = ($res = $aExp[$t]) ? "[PASS]" : "[FAIL]"
    ConsoleWrite("Test " & $t+1 & ": " & $res & " " & $ok & @CRLF)
Next
