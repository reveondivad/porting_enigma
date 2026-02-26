\\ Enigma Cipher - PARI/GP
\\ Number theory / algebra system
\\ Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
\\ PeopleTec Inc. - Guinness World Record Attempt 2026

fwdI  = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
fwdII = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
fwdIII= [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];
bwdI  = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
bwdII = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
bwdIII= [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12];
reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
notch = [16, 4, 21];

mod26(n) = { my(m = n % 26); if(m < 0, m + 26, m); }

getFwd(r, i) = { if(r==0, fwdI[i+1], if(r==1, fwdII[i+1], fwdIII[i+1])); }
getBwd(r, i) = { if(r==0, bwdI[i+1], if(r==1, bwdII[i+1], bwdIII[i+1])); }

passFwd(rotor, offset, ch) = {
  my(inp = mod26(ch + offset));
  my(out = getFwd(rotor, inp));
  mod26(out - offset);
}

passBwd(rotor, offset, ch) = {
  my(inp = mod26(ch + offset));
  my(out = getBwd(rotor, inp));
  mod26(out - offset);
}

enigma(r0, r1, r2, k0, k1, k2, msg) = {
  my(o0=k0, o1=k1, o2=k2);
  my(n1=notch[r1+1], n2=notch[r2+1]);
  my(pb=vector(26, i, i-1));
  my(result="");
  for(i=1, #msg,
    my(ch = Vecsmall(msg)[i] - 65);
    my(mid = (o1 == n1));
    my(atn = (o2 == n2));
    o2 = mod26(o2 + 1);
    if(atn || mid, o1 = mod26(o1 + 1));
    if(mid, o0 = mod26(o0 + 1));
    my(c = pb[ch+1]);
    c = passFwd(r2, o2, c);
    c = passFwd(r1, o1, c);
    c = passFwd(r0, o0, c);
    c = reflector[c+1];
    c = passBwd(r0, o0, c);
    c = passBwd(r1, o1, c);
    c = passBwd(r2, o2, c);
    c = pb[c+1];
    result = concat(result, Strchr(c + 65));
  );
  result;
}

print("Enigma Cipher - PARI/GP\n");
print("Test 1: ", enigma(0,1,2,0,0,0,"AAAAA"), " expected BDZGO\n");
print("Test 2: ", enigma(0,1,2,0,0,0,"HELLOWORLD"), " expected ILBDAAMTAZ\n");
print("Test 3: ", enigma(0,1,2,0,0,0,"ATTACKATDAWN"), " expected BZHGNOCRRTCM\n");
print("Test 4: ", enigma(0,1,2,12,2,10,"HELLOWORLD"), " expected DLTBBQVPQV\n");
print("Test 5: ", enigma(2,0,1,0,0,0,"HELLOWORLD"), " expected KZHDFQYHXT\n");
