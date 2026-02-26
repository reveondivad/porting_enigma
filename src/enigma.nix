# Enigma Cipher - Nix Expression Language
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026
# Usage: nix eval -f enigma.nix

let
  fwdI = [4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9];
  fwdII = [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4];
  fwdIII = [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14];
  bwdI = [20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9];
  bwdII = [0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18];
  bwdIII = [19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12];
  reflector = [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19];
  notches = [16 4 21];

  lib = builtins;

  mod26 = n: let m = lib.mod (lib.mod n 26 + 26) 26; in m;

  elemAt = list: idx: lib.elemAt list idx;

  getFwd = r: if r == 0 then fwdI else if r == 1 then fwdII else fwdIII;
  getBwd = r: if r == 0 then bwdI else if r == 1 then bwdII else bwdIII;

  passFwd = rotor: offset: ch:
    let inp = mod26 (ch + offset);
        out = elemAt (getFwd rotor) inp;
    in mod26 (out - offset + 26);

  passBwd = rotor: offset: ch:
    let inp = mod26 (ch + offset);
        out = elemAt (getBwd rotor) inp;
    in mod26 (out - offset + 26);

  step = state:
    let o0 = state.o0; o1 = state.o1; o2 = state.o2;
        n1 = state.n1; n2 = state.n2;
        mid = o1 == n1;
        newO2 = mod26 (o2 + 1);
        newO1 = if mid || o2 == n2 then mod26 (o1 + 1) else o1;
        newO0 = if mid then mod26 (o0 + 1) else o0;
    in state // { o0 = newO0; o1 = newO1; o2 = newO2; };

  encryptChar = state: ch:
    let s = step state;
        r0 = s.r0; r1 = s.r1; r2 = s.r2;
        c0 = elemAt s.pb ch;
        c1 = passFwd r2 s.o2 c0;
        c2 = passFwd r1 s.o1 c1;
        c3 = passFwd r0 s.o0 c2;
        c4 = elemAt reflector c3;
        c5 = passBwd r0 s.o0 c4;
        c6 = passBwd r1 s.o1 c5;
        c7 = passBwd r2 s.o2 c6;
        c8 = elemAt s.pb c7;
    in { char = c8; state = s; };

  mkState = rotors: key:
    let r0 = elemAt rotors 0; r1 = elemAt rotors 1; r2 = elemAt rotors 2;
    in {
      inherit r0 r1 r2;
      o0 = elemAt key 0; o1 = elemAt key 1; o2 = elemAt key 2;
      n1 = elemAt notches r1; n2 = elemAt notches r2;
      pb = lib.genList (i: i) 26;
    };

  encryptAll = state: chars:
    if chars == [] then { result = []; inherit state; }
    else let
      head = lib.head chars;
      tail = lib.tail chars;
      enc = encryptChar state head;
      rest = encryptAll enc.state tail;
    in { result = [enc.char] ++ rest.result; state = rest.state; };

  encrypt = rotors: key: msg:
    let chars = map (c: c - 65) (lib.stringToCharacters msg);
        state = mkState rotors key;
        result = encryptAll state chars;
    in lib.concatStringsSep "" (map (c: lib.substring (c + 65) 1 "ABCDEFGHIJKLMNOPQRSTUVWXYZ") result.result);

in {
  test1 = encrypt [0 1 2] [0 0 0] "AAAAA";       # BDZGO
  test2 = encrypt [0 1 2] [0 0 0] "HELLOWORLD";   # ILBDAAMTAZ
  test3 = encrypt [0 1 2] [0 0 0] "ATTACKATDAWN"; # BZHGNOCRRTCM
}
