/* Enigma cipher in B (Ken Thompson's language, predecessor to C) */
main() {
  auto rf1, rf2, rf3, rb1, rb2, rb3, ref, notch;
  auto pos, text, i, c, j, mid, result;
  
  /* Rotor I forward */
  rf1 = vec(26);
  rf1[0]=4; rf1[1]=10; rf1[2]=12; rf1[3]=5; rf1[4]=11; rf1[5]=6;
  rf1[6]=3; rf1[7]=16; rf1[8]=21; rf1[9]=25; rf1[10]=13; rf1[11]=19;
  rf1[12]=14; rf1[13]=22; rf1[14]=24; rf1[15]=7; rf1[16]=23; rf1[17]=20;
  rf1[18]=18; rf1[19]=15; rf1[20]=0; rf1[21]=8; rf1[22]=1; rf1[23]=17;
  rf1[24]=2; rf1[25]=9;
  
  /* Reflector B */
  ref = vec(26);
  ref[0]=24; ref[1]=17; ref[2]=20; ref[3]=7; ref[4]=16; ref[5]=18;
  ref[6]=11; ref[7]=3; ref[8]=15; ref[9]=23; ref[10]=13; ref[11]=6;
  ref[12]=14; ref[13]=10; ref[14]=12; ref[15]=8; ref[16]=4; ref[17]=1;
  ref[18]=5; ref[19]=25; ref[20]=2; ref[21]=22; ref[22]=21; ref[23]=9;
  ref[24]=0; ref[25]=19;
  
  notch = vec(3);
  notch[0] = 16; notch[1] = 4; notch[2] = 21;
  
  pos = vec(3);
  pos[0] = 0; pos[1] = 0; pos[2] = 0;
  
  printf("Enigma B language implementation*n");
}

mod26(n) {
  auto m;
  m = n - (n/26)*26;
  if (m < 0) m = m + 26;
  return(m);
}
