/* Enigma Cipher - Yorick
   Scientific computing language from LLNL
   Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
   PeopleTec Inc. - Guinness World Record Attempt 2026 */

fwd = array(long, 26, 3);
fwd(,1) = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
fwd(,2) = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
fwd(,3) = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];

bwd = array(long, 26, 3);
bwd(,1) = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
bwd(,2) = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
bwd(,3) = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12];

ref = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
notches = [16, 4, 21];

func mod26(n) { return ((n % 26) + 26) % 26; }

func pass_fwd(rotor, offset, ch)
{
  inp = mod26(ch + offset);
  out = fwd(inp+1, rotor+1);
  return mod26(out - offset);
}

func pass_bwd(rotor, offset, ch)
{
  inp = mod26(ch + offset);
  out = bwd(inp+1, rotor+1);
  return mod26(out - offset);
}

func enigma_encrypt(r0, r1, r2, k0, k1, k2, msg)
{
  pb = indgen(0:25);
  o0 = k0; o1 = k1; o2 = k2;
  n0 = notches(r0+1); n1 = notches(r1+1); n2 = notches(r2+1);
  result = "";

  for (i = 1; i <= strlen(msg); i++) {
    ch = (*pointer(strpart(msg, i:i)))(1) - 65;
    mid = (o1 == n1);
    atn = (o2 == n2);
    o2 = mod26(o2 + 1);
    if (atn || mid) o1 = mod26(o1 + 1);
    if (mid) o0 = mod26(o0 + 1);
    c = pb(ch+1);
    c = pass_fwd(r2, o2, c);
    c = pass_fwd(r1, o1, c);
    c = pass_fwd(r0, o0, c);
    c = ref(c+1);
    c = pass_bwd(r0, o0, c);
    c = pass_bwd(r1, o1, c);
    c = pass_bwd(r2, o2, c);
    c = pb(c+1);
    result += string(&char(c + 65));
  }
  return result;
}

write, "Enigma Cipher - Yorick";
write, "Test 1:", enigma_encrypt(0,1,2, 0,0,0, "AAAAA"), " expected BDZGO";
write, "Test 2:", enigma_encrypt(0,1,2, 0,0,0, "HELLOWORLD"), " expected ILBDAAMTAZ";
write, "Test 3:", enigma_encrypt(0,1,2, 0,0,0, "ATTACKATDAWN"), " expected BZHGNOCRRTCM";
write, "Test 4:", enigma_encrypt(0,1,2, 12,2,10, "HELLOWORLD"), " expected DLTBBQVPQV";
write, "Test 5:", enigma_encrypt(2,0,1, 0,0,0, "HELLOWORLD"), " expected KZHDFQYHXT";
