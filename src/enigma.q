/ Enigma Cipher - Q (kdb+ query language)
/ Vector processing language for time-series data

fI: 4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9;
fII: 0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4;
fIII: 1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14;

bI: 20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9;
bII: 0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18;
bIII: 19 0 6 1 15 2 18 3 16 4 20 9 21 13 25 7 24 8 23 5 22 11 17 12 14 10;

ntch: 16 4 21;
refl: 24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19;

fwd: (fI;fII;fIII);
bwd: (bI;bII;bIII);

m26:{mod[;26] 26+mod[x;26]};

pfwd:{[r;o;c] m26[fwd[r] m26[c+o] - o]};
pbwd:{[r;o;c] m26[bwd[r] m26[c+o] - o]};

mkpb:{[pairs] p:til 26; {[p;ab] p[ab 0]:ab 1; p[ab 1]:ab 0; p}/[p;pairs]};

enigma:{[r0;r1;r2;k0;k1;k2;pairs;msg]
  plug:mkpb pairs;
  st:(k0;k1;k2);
  r:{[st;ch]
    o0:st 0; o1:st 1; o2:st 2; res:st 3;
    mid:o1=ntch r1; atn:o2=ntch r2;
    o2:m26 o2+1;
    o1:$[atn|mid;m26 o1+1;o1];
    o0:$[mid;m26 o0+1;o0];
    c:plug ch;
    c:pfwd[r2;o2;c]; c:pfwd[r1;o1;c]; c:pfwd[r0;o0;c];
    c:refl c;
    c:pbwd[r0;o0;c]; c:pbwd[r1;o1;c]; c:pbwd[r2;o2;c];
    c:plug c;
    (o0;o1;o2;res,c)
  };
  `char$ 65+ r/[(st,enlist `long$());msg] 3};

-1 "Enigma Cipher - Q";
-1 "Test 1: ",enigma[0;1;2;0;0;0;();5#0]," (expected BDZGO)";
-1 "Test 2: ",enigma[0;1;2;0;0;0;();`long$"HELLOWORLD"-65]," (expected ILBDAAMTAZ)";
-1 "Test 3: ",enigma[0;1;2;0;0;0;();`long$"ATTACKATDAWN"-65]," (expected BZHGNOCRRTCM)";
-1 "Test 4: ",enigma[0;1;2;12;2;10;();`long$"HELLOWORLD"-65]," (expected DLTBBQVPQV)";
-1 "Test 5: ",enigma[2;0;1;0;0;0;();`long$"HELLOWORLD"-65]," (expected KZHDFQYHXT)";
-1 "Test 6: ",enigma[0;1;2;0;0;0;(0 1;2 3;4 5);`long$"HELLOWORLD"-65]," (expected IKACBBMTBF)";
