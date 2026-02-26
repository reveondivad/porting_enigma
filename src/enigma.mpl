# Enigma Cipher - Maple
# Computer algebra system / mathematical programming
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

fwdI := [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]:
fwdII := [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]:
fwdIII := [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]:
bwdI := [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]:
bwdII := [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]:
bwdIII := [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]:
reflector := [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]:
notches := [16, 4, 21]:

mod26 := proc(n) ((n mod 26) + 26) mod 26 end proc:

getFwd := proc(r, i)
  if r = 0 then fwdI[i+1]
  elif r = 1 then fwdII[i+1]
  else fwdIII[i+1]
  end if
end proc:

getBwd := proc(r, i)
  if r = 0 then bwdI[i+1]
  elif r = 1 then bwdII[i+1]
  else bwdIII[i+1]
  end if
end proc:

passFwd := proc(rotor, offset, ch)
  local inp, out;
  inp := mod26(ch + offset);
  out := getFwd(rotor, inp);
  mod26(out - offset)
end proc:

passBwd := proc(rotor, offset, ch)
  local inp, out;
  inp := mod26(ch + offset);
  out := getBwd(rotor, inp);
  mod26(out - offset)
end proc:

enigma_encrypt := proc(r0, r1, r2, k0, k1, k2, msg)
  local pb, o0, o1, o2, n1, n2, result, i, ch, mid, atn, c;
  pb := [seq(i, i=0..25)];
  o0 := k0; o1 := k1; o2 := k2;
  n1 := notches[r1+1]; n2 := notches[r2+1];
  result := "";
  for i from 1 to length(msg) do
    ch := convert(substring(msg, i..i), bytes)[1] - 65;
    mid := evalb(o1 = n1);
    atn := evalb(o2 = n2);
    o2 := mod26(o2 + 1);
    if atn or mid then o1 := mod26(o1 + 1) end if;
    if mid then o0 := mod26(o0 + 1) end if;
    c := pb[ch+1];
    c := passFwd(r2, o2, c);
    c := passFwd(r1, o1, c);
    c := passFwd(r0, o0, c);
    c := reflector[c+1];
    c := passBwd(r0, o0, c);
    c := passBwd(r1, o1, c);
    c := passBwd(r2, o2, c);
    c := pb[c+1];
    result := cat(result, convert([c + 65], bytes));
  end do;
  result
end proc:

printf("Enigma Cipher - Maple\n");
printf("Test 1: %s (expected BDZGO)\n", enigma_encrypt(0,1,2, 0,0,0, "AAAAA"));
printf("Test 2: %s (expected ILBDAAMTAZ)\n", enigma_encrypt(0,1,2, 0,0,0, "HELLOWORLD"));
printf("Test 3: %s (expected BZHGNOCRRTCM)\n", enigma_encrypt(0,1,2, 0,0,0, "ATTACKATDAWN"));
printf("Test 4: %s (expected DLTBBQVPQV)\n", enigma_encrypt(0,1,2, 12,2,10, "HELLOWORLD"));
printf("Test 5: %s (expected KZHDFQYHXT)\n", enigma_encrypt(2,0,1, 0,0,0, "HELLOWORLD"));
