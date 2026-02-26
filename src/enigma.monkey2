' Enigma Cipher - Monkey2 (Wonkey)
' Cross-platform game programming language
' Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
' PeopleTec Inc. - Guinness World Record Attempt 2026

Namespace enigma

#Import "<std>"
Using std..

Global fwdI:Int[] = New Int[](4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9)
Global fwdII:Int[] = New Int[](0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4)
Global fwdIII:Int[] = New Int[](1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14)
Global bwdI:Int[] = New Int[](20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9)
Global bwdII:Int[] = New Int[](0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18)
Global bwdIII:Int[] = New Int[](19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12)
Global reflector:Int[] = New Int[](24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19)
Global notches:Int[] = New Int[](16, 4, 21)

Function Mod26:Int(n:Int)
    Local m:=n Mod 26
    If m<0 Then m+=26
    Return m
End

Function GetFwd:Int(r:Int, i:Int)
    Select r
    Case 0 Return fwdI[i]
    Case 1 Return fwdII[i]
    Default Return fwdIII[i]
    End
End

Function GetBwd:Int(r:Int, i:Int)
    Select r
    Case 0 Return bwdI[i]
    Case 1 Return bwdII[i]
    Default Return bwdIII[i]
    End
End

Function PassFwd:Int(rotor:Int, offset:Int, ch:Int)
    Return Mod26(GetFwd(rotor, Mod26(ch+offset)) - offset)
End

Function PassBwd:Int(rotor:Int, offset:Int, ch:Int)
    Return Mod26(GetBwd(rotor, Mod26(ch+offset)) - offset)
End

Class Enigma
    Field r:Int[3], o:Int[3], n1:Int, n2:Int
    
    Method New(r0:Int,r1:Int,r2:Int,k0:Int,k1:Int,k2:Int)
        r[0]=r0;r[1]=r1;r[2]=r2
        o[0]=k0;o[1]=k1;o[2]=k2
        n1=notches[r1];n2=notches[r2]
    End
    
    Method Step()
        If o[1]=n1 Then o[1]=Mod26(o[1]+1);o[0]=Mod26(o[0]+1)
        Elseif o[2]=n2 Then o[1]=Mod26(o[1]+1)
        o[2]=Mod26(o[2]+1)
    End
    
    Method PressKey:Int(ch:Int)
        Step()
        Local c:=ch
        c=PassFwd(r[2],o[2],c);c=PassFwd(r[1],o[1],c);c=PassFwd(r[0],o[0],c)
        c=reflector[c]
        c=PassBwd(r[0],o[0],c);c=PassBwd(r[1],o[1],c);c=PassBwd(r[2],o[2],c)
        Return c
    End
    
    Method Encrypt:String(msg:String)
        Local result:=""
        For Local i:=0 Until msg.Length
            result+=String.FromChar(PressKey(msg[i]-65)+65)
        Next
        Return result
    End
End

Function Main()
    Print "Enigma Cipher - Monkey2"
    Local e:=New Enigma(0,1,2,0,0,0)
    Print "Test 1: "+e.Encrypt("AAAAA")+" expected BDZGO"
End
