// Enigma cipher in HolyC (TempleOS)
I64 rotor_fwd[3][26] = {
  {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9},
  {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4},
  {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14}
};
I64 rotor_bwd[3][26] = {
  {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9},
  {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18},
  {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12}
};
I64 reflector[26] = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
I64 notches[3] = {16, 4, 21};

I64 Mod26(I64 n) { return ((n % 26) + 26) % 26; }

U0 Enigma(U8 *text) {
  I64 pos[3] = {0, 0, 0};
  I64 i, c, j, len = StrLen(text);
  U8 result[256];
  I64 ri = 0;

  for (i = 0; i < len; i++) {
    c = text[i];
    if (c >= 'a' && c <= 'z') c -= 32;
    if (c < 'A' || c > 'Z') continue;
    c -= 'A';

    // Step rotors with double-stepping
    Bool mid = pos[1] == notches[1];
    if (pos[2] == notches[2]) pos[2] = Mod26(pos[2] + 1);
    if (mid || pos[2] == notches[2]) pos[1] = Mod26(pos[1] + 1);
    pos[2] = Mod26(pos[2] + 1);

    // Forward through rotors
    for (j = 2; j >= 0; j--)
      c = Mod26(rotor_fwd[j][Mod26(c + pos[j])] - pos[j]);
    c = reflector[c];
    // Backward through rotors
    for (j = 0; j <= 2; j++)
      c = Mod26(rotor_bwd[j][Mod26(c + pos[j])] - pos[j]);

    result[ri++] = c + 'A';
  }
  result[ri] = 0;
  Print("%s\n", result);
}

Enigma("HELLOWORLD");
