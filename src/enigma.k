/ Enigma Cipher - K (kdb+/q)
/ Array language for financial computing

fI: 4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9
fII: 0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4
fIII: 1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14

bI: 20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9
bII: 0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18
bIII: 19 0 6 1 15 2 18 3 16 4 20 9 21 13 25 7 24 8 23 5 22 11 17 12 14 10

ntch: 16 4 21
refl: 24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19

fwd: (fI;fII;fIII)
bwd: (bI;bII;bIII)

m26:{(x mod 26+26) mod 26}

/ Pass through rotor forward
pf:{[r;o;c] m26[(fwd r) m26[c+o] - o]}

/ Pass through rotor backward
pb:{[r;o;c] m26[(bwd r) m26[c+o] - o]}

/ Make plugboard from pairs (list of 2-element lists)
mkpb:{[pairs] p:til 26; {[p;ab] p[ab 0]:ab 1; p[ab 1]:ab 0; p}/[p;pairs]}

/ Encrypt message
enc:{[r0;r1;r2;k0;k1;k2;pairs;msg]
  plug: mkpb pairs;
  o: k0 k1 k2;
  res: ();
  {[state;ch]
    o: state 0; res: state 1;
    mid: o[1]=ntch r1;
    atn: o[2]=ntch r2;
    o[2]: m26 o[2]+1;
    if[atn|mid; o[1]: m26 o[1]+1];
    if[mid; o[0]: m26 o[0]+1];
    c: plug ch;
    c: pf[r2;o 2;c];
    c: pf[r1;o 1;c];
    c: pf[r0;o 0;c];
    c: refl c;
    c: pb[r0;o 0;c];
    c: pb[r1;o 1;c];
    c: pb[r2;o 2;c];
    c: plug c;
    (o;res,c)
  }/[(o;());msg-65] 1;
  `char$res+65}

/ Tests
-1 "Enigma Cipher - K";
-1 "Test 1: ",(enc[0;1;2;0;0;0;();`char$65+5#0])," (expected BDZGO)";
-1 "Test 2: ",(enc[0;1;2;0;0;0;();"HELLOWORLD"-65])," (expected ILBDAAMTAZ)";
-1 "Test 3: ",(enc[0;1;2;0;0;0;();"ATTACKATDAWN"-65])," (expected BZHGNOCRRTCM)";
-1 "Test 4: ",(enc[0;1;2;12;2;10;();"HELLOWORLD"-65])," (expected DLTBBQVPQV)";
-1 "Test 5: ",(enc[2;0;1;0;0;0;();"HELLOWORLD"-65])," (expected KZHDFQYHXT)";
-1 "Test 6: ",(enc[0;1;2;0;0;0;(0 1;2 3;4 5);"HELLOWORLD"-65])," (expected IKACBBMTBF)";
