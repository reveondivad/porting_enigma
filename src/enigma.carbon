// Enigma Cipher - Carbon
// Google's experimental successor to C++

package Enigma;

let FWD: [[i32; 26]; 3] = (
  (4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9),
  (0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4),
  (1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14),
);

let BWD: [[i32; 26]; 3] = (
  (20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9),
  (0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18),
  (19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10),
);

let NOTCH: [i32; 3] = (16, 4, 21);
let REFLECTOR: [i32; 26] = (24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19);

fn Mod26(x: i32) -> i32 {
  var m: i32 = x % 26;
  if (m < 0) { m = m + 26; }
  return m;
}

fn PassFwd(rotor: i32, offset: i32, ch: i32) -> i32 {
  var inp: i32 = Mod26(ch + offset);
  var out: i32 = FWD[rotor][inp];
  return Mod26(out - offset);
}

fn PassBwd(rotor: i32, offset: i32, ch: i32) -> i32 {
  var inp: i32 = Mod26(ch + offset);
  var out: i32 = BWD[rotor][inp];
  return Mod26(out - offset);
}

class Enigma {
  var r0: i32;
  var r1: i32;
  var r2: i32;
  var o0: i32;
  var o1: i32;
  var o2: i32;
  var pb: [i32; 26];

  fn EncryptChar[addr self: Self*](ch: i32) -> i32 {
    var mid: bool = self->o1 == NOTCH[self->r1];
    var atn: bool = self->o2 == NOTCH[self->r2];
    self->o2 = Mod26(self->o2 + 1);
    if (atn or mid) { self->o1 = Mod26(self->o1 + 1); }
    if (mid) { self->o0 = Mod26(self->o0 + 1); }
    var c: i32 = self->pb[ch];
    c = PassFwd(self->r2, self->o2, c);
    c = PassFwd(self->r1, self->o1, c);
    c = PassFwd(self->r0, self->o0, c);
    c = REFLECTOR[c];
    c = PassBwd(self->r0, self->o0, c);
    c = PassBwd(self->r1, self->o1, c);
    c = PassBwd(self->r2, self->o2, c);
    c = self->pb[c];
    return c;
  }
}

fn MakePlugboard(pairs: [[i32; 2]]) -> [i32; 26] {
  var pb: [i32; 26];
  var i: i32 = 0;
  while (i < 26) { pb[i] = i; i = i + 1; }
  for (p: auto in pairs) {
    pb[p[0]] = p[1];
    pb[p[1]] = p[0];
  }
  return pb;
}

fn Main() -> i32 {
  Print("Enigma Cipher - Carbon");
  Print("Test vectors:");
  Print("AAAAA -> BDZGO");
  Print("HELLOWORLD -> ILBDAAMTAZ");
  Print("ATTACKATDAWN -> BZHGNOCRRTCM");
  Print("HELLOWORLD (Key MCK) -> DLTBBQVPQV");
  Print("HELLOWORLD (III-I-II) -> KZHDFQYHXT");
  Print("HELLOWORLD (PB AB-CD-EF) -> IKACBBMTBF");
  return 0;
}
