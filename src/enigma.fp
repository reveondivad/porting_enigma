{ Enigma Cipher - FP (Backus FP)
  Functional Programming language by John Backus (1977)
  Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
  PeopleTec Inc. - Guinness World Record Attempt 2026 }

Def fwdI = <4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9>
Def fwdII = <0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4>
Def fwdIII = <1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14>
Def reflector = <24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19>
Def notches = <16, 4, 21>

{ mod26: n -> (n%26+26)%26 }
Def mod26 = %26 -> m ; (m < 0 -> m + 26 ; m)

{ getFwd: <r, i> -> fwdI[i] | fwdII[i] | fwdIII[i] }
Def getFwd = (eq0 o 1 -> 1 o fwdI o 2 ; eq1 o 1 -> 1 o fwdII o 2 ; 1 o fwdIII o 2)

{ passFwd: <rotor, offset, ch> -> mod26(getFwd(rotor, mod26(ch+offset)) - offset) }
Def passFwd = mod26 o (- o [getFwd o [1, mod26 o + o [3, 2]], 2])

{ Test: AAAAA -> BDZGO, HELLOWORLD -> ILBDAAMTAZ }
