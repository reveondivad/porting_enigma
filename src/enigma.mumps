; Enigma Cipher - MUMPS (M)
; Massachusetts General Hospital Utility Multi-Programming System
; Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
; PeopleTec Inc. - Guinness World Record Attempt 2026

ENIGMA
 ; Initialize rotor wirings using local arrays
 N I
 F I=0:1:25 S FI(I)=$P("4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9",",",I+1)
 F I=0:1:25 S FII(I)=$P("0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4",",",I+1)
 F I=0:1:25 S FIII(I)=$P("1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14",",",I+1)
 F I=0:1:25 S BI(I)=$P("20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9",",",I+1)
 F I=0:1:25 S BII(I)=$P("0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18",",",I+1)
 F I=0:1:25 S BIII(I)=$P("19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12",",",I+1)
 F I=0:1:25 S REF(I)=$P("24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19",",",I+1)
 S NOTCH(0)=16,NOTCH(1)=4,NOTCH(2)=21
 ;
 ; Encrypt AAAAA with rotors 0,1,2 offsets 0,0,0
 S R(0)=0,R(1)=1,R(2)=2
 S O(0)=0,O(1)=0,O(2)=0
 S N1=NOTCH(R(1)),N2=NOTCH(R(2))
 S MSG="AAAAA",RES=""
 F I=1:1:$L(MSG) D
 . ; Step
 . I O(1)=N1 S O(1)=$$MOD26(O(1)+1),O(0)=$$MOD26(O(0)+1)
 . E  I O(2)=N2 S O(1)=$$MOD26(O(1)+1)
 . S O(2)=$$MOD26(O(2)+1)
 . ; Encrypt char
 . S C=$A($E(MSG,I))-65
 . S C=$$PFWD(R(2),O(2),C),C=$$PFWD(R(1),O(1),C),C=$$PFWD(R(0),O(0),C)
 . S C=REF(C)
 . S C=$$PBWD(R(0),O(0),C),C=$$PBWD(R(1),O(1),C),C=$$PBWD(R(2),O(2),C)
 . S RES=RES_$C(C+65)
 W "Enigma Cipher - MUMPS",!
 W "Test: "_RES_" expected BDZGO",!
 Q
 ;
MOD26(N)
 N M S M=N#26
 I M<0 S M=M+26
 Q M
 ;
GFWD(R,I)
 I R=0 Q FI(I)
 I R=1 Q FII(I)
 Q FIII(I)
 ;
GBWD(R,I)
 I R=0 Q BI(I)
 I R=1 Q BII(I)
 Q BIII(I)
 ;
PFWD(RT,OF,CH)
 N INP S INP=$$MOD26(CH+OF)
 Q $$MOD26($$GFWD(RT,INP)-OF)
 ;
PBWD(RT,OF,CH)
 N INP S INP=$$MOD26(CH+OF)
 Q $$MOD26($$GBWD(RT,INP)-OF)
