ENIGMA ; Wehrmacht Enigma I cipher in MUMPS/M
 ; 3 rotors, Reflector B, plugboard, double-stepping
 ;
 ; MUMPS (Massachusetts General Hospital Utility Multi-Programming System)
 ; Used primarily in healthcare/VA systems
 ;
INIT ;
 ; Rotor wirings stored as strings
 S ALPHA="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 S RI="EKMFLGDQVZNTOWYHXUSPAIBRCJ"
 S RII="AJDKSIRUXBLHWTMCQGZNPYFVOE"
 S RIII="BDFHJLCPRTXVZNYEIWGAKMUSQO"
 S REFL="YRUHQSLDPXNGOKMIEBFZCWVJAT"
 ;
 ; Build inverse rotors
 F I=1:1:26 S $E(RII(1),$F(ALPHA,$E(RI,I)))=$E(ALPHA,I)
 F I=1:1:26 S $E(RIII(1),$F(ALPHA,$E(RII,I)))=$E(ALPHA,I)
 F I=1:1:26 S $E(RI(1),$F(ALPHA,$E(RIII,I)))=$E(ALPHA,I)
 ;
 ; Notch positions (1-based)
 S NOTCH(1)=17 ; Rotor I: Q
 S NOTCH(2)=5  ; Rotor II: E
 S NOTCH(3)=22 ; Rotor III: V
 Q
 ;
C2I(C) ; Char to 0-based index
 Q $F(ALPHA,C)-2
 ;
I2C(I) ; 0-based index to char
 Q $E(ALPHA,I+1)
 ;
MOD26(N) ; Modulo 26 (handles negatives)
 N R S R=N#26
 I R<0 S R=R+26
 Q R
 ;
FWD(CH,WIRING,OFF) ; Forward through rotor
 N IDX,OUT
 S IDX=$$MOD26(CH+OFF)
 S OUT=$$C2I($E(WIRING,IDX+1))
 Q $$MOD26(OUT-OFF)
 ;
BWD(CH,WIRING,OFF) ; Backward through rotor
 N IDX,POS
 S IDX=$$MOD26(CH+OFF)
 S POS=$F(WIRING,$E(ALPHA,IDX+1))-2
 Q $$MOD26(POS-OFF)
 ;
PLUG(CH,PB) ; Plugboard lookup
 Q $E(PB,CH+1)-65+0 ; PB stored as ASCII values
 ; Actually let's use a simpler approach
 N I,C,P
 S C=$$I2C(CH)
 I PB="" Q CH
 F I=1:3:$L(PB) D  Q:$D(RES)
 . S P=$E(PB,I,I+1)
 . I $E(P,1)=C S RES=$$C2I($E(P,2))
 . I $E(P,2)=C S RES=$$C2I($E(P,1))
 I $D(RES) Q RES
 Q CH
 ;
MKPB(PAIRS) ; Make plugboard array (26 element)
 N PB,I,P,A,B
 F I=0:1:25 S PB(I)=I
 I PAIRS="" Q
 N J S J=1
 F  Q:J>$L(PAIRS)  D
 . S A=$$C2I($E(PAIRS,J))
 . S B=$$C2I($E(PAIRS,J+1))
 . S PB(A)=B,PB(B)=A
 . S J=J+3
 Q
 ;
ENCRYPT(ROTORS,KEY,PLUGPAIRS,TEXT) ; Main encrypt function
 ; ROTORS = "1 2 3" (space separated rotor numbers)
 ; KEY = "AAA" (3 char key)
 ; PLUGPAIRS = "ABCDEF" (pairs concatenated) or ""
 ; TEXT = plaintext
 ;
 D INIT
 ;
 N R,M,L,OR,OM,OL,WR,WM,WL,NR,NM
 ; Parse rotors
 S L=$P(ROTORS," ",1)
 S M=$P(ROTORS," ",2)
 S R=$P(ROTORS," ",3)
 ;
 ; Select wirings
 I R=1 S WR=RI,NR=NOTCH(1)
 I R=2 S WR=RII,NR=NOTCH(2)
 I R=3 S WR=RIII,NR=NOTCH(3)
 I M=1 S WM=RI,NM=NOTCH(1)
 I M=2 S WM=RII,NM=NOTCH(2)
 I M=3 S WM=RIII,NM=NOTCH(3)
 I L=1 S WL=RI
 I L=2 S WL=RII
 I L=3 S WL=RIII
 ;
 ; Build inverse wirings
 N WRI,WMI,WLI
 F I=1:1:26 S $E(WRI,$F(ALPHA,$E(WR,I)))=$E(ALPHA,I)
 F I=1:1:26 S $E(WMI,$F(ALPHA,$E(WM,I)))=$E(ALPHA,I)
 F I=1:1:26 S $E(WLI,$F(ALPHA,$E(WL,I)))=$E(ALPHA,I)
 ;
 ; Initial offsets
 S OR=$$C2I($E(KEY,3))
 S OM=$$C2I($E(KEY,2))
 S OL=$$C2I($E(KEY,1))
 ;
 ; Build plugboard
 D MKPB(PLUGPAIRS)
 ;
 ; Encrypt each character
 N RESULT,I,CH,MIDN,RATN,C
 S RESULT=""
 F I=1:1:$L(TEXT) D
 . S CH=$$C2I($E(TEXT,I))
 . ;
 . ; Step rotors
 . S MIDN=(OM=(NM-1))
 . S RATN=(OR=(NR-1))
 . I MIDN S OL=$$MOD26(OL+1)
 . I MIDN!RATN S OM=$$MOD26(OM+1)
 . S OR=$$MOD26(OR+1)
 . ;
 . ; Plugboard
 . S C=PB(CH)
 . ;
 . ; Forward through rotors R M L
 . S C=$$FWD(C,WR,OR)
 . S C=$$FWD(C,WM,OM)
 . S C=$$FWD(C,WL,OL)
 . ;
 . ; Reflector
 . S C=$$C2I($E(REFL,C+1))
 . ;
 . ; Backward through rotors L M R
 . S C=$$FWD(C,WLI,OL)
 . S C=$$FWD(C,WMI,OM)
 . S C=$$FWD(C,WRI,OR)
 . ;
 . ; Plugboard
 . S C=PB(C)
 . ;
 . S RESULT=RESULT_$$I2C(C)
 ;
 Q RESULT
 ;
TEST ; Run test vectors
 W "Enigma MUMPS Implementation",!
 W "===========================",!
 ;
 N R
 S R=$$ENCRYPT("1 2 3","AAA","","AAAAA")
 W "Test 1: ",R," ",$S(R="BDZGO":"PASS",1:"FAIL"),!
 ;
 S R=$$ENCRYPT("1 2 3","AAA","","HELLOWORLD")
 W "Test 2: ",R," ",$S(R="ILBDAAMTAZ":"PASS",1:"FAIL"),!
 ;
 S R=$$ENCRYPT("1 2 3","AAA","","ATTACKATDAWN")
 W "Test 3: ",R," ",$S(R="BZHGNOCRRTCM":"PASS",1:"FAIL"),!
 ;
 S R=$$ENCRYPT("1 2 3","MCK","","HELLOWORLD")
 W "Test 4: ",R," ",$S(R="DLTBBQVPQV":"PASS",1:"FAIL"),!
 ;
 S R=$$ENCRYPT("3 1 2","AAA","","HELLOWORLD")
 W "Test 5: ",R," ",$S(R="KZHDFQYHXT":"PASS",1:"FAIL"),!
 ;
 S R=$$ENCRYPT("1 2 3","AAA","ABCDEF","HELLOWORLD")
 W "Test 6: ",R," ",$S(R="IKACBBMTBF":"PASS",1:"FAIL"),!
 ;
 Q