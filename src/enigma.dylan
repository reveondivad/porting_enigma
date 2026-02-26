Module: enigma
Synopsis: Enigma Machine - Dylan Implementation
Author: PeopleTec Inc.
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// Guinness World Record Attempt 2026

define constant $fwd :: <simple-object-vector> =
  vector("EKMFLGDQVZNTOWYHXUSPAIBRCJ",
         "AJDKSIRUXBLHWTMCQGZNPYFVOE",
         "BDFHJLCPRTXVZNYEIWGAKMUSQO");

define constant $bwd :: <simple-object-vector> =
  vector("UWYGADFPVZBECKMTHXSLRINQOJ",
         "AJPCZWRLFBDKOTYUQGENHXMIVS",
         "TAGBPCSDQEUFVNZHYIXJWLRKOM");

define constant $ref :: <string> = "YRUHQSLDPXNGOKMIEBFZCWVJAT";
define constant $notch :: <simple-object-vector> = vector(16, 4, 21);

define function mod26 (n :: <integer>) => (r :: <integer>)
  let m = modulo(n, 26);
  if (m < 0) m + 26 else m end
end function;

define class <enigma> (<object>)
  slot rotors :: <simple-object-vector>, init-keyword: rotors:;
  slot offsets :: <simple-object-vector>, init-keyword: offsets:;
  slot notches :: <simple-object-vector>, init-keyword: notches:;
  slot plugboard :: <simple-object-vector>, init-keyword: plugboard:;
end class;

define function make-enigma
    (r1 :: <integer>, r2 :: <integer>, r3 :: <integer>,
     k1 :: <character>, k2 :: <character>, k3 :: <character>,
     #key plug :: <string> = "")
 => (e :: <enigma>)
  let ri = vector(r1 - 1, r2 - 1, r3 - 1);
  let pb = make(<simple-object-vector>, size: 26);
  for (i from 0 below 26) pb[i] := i end;
  make(<enigma>,
       rotors: ri,
       offsets: vector(as(<integer>, k1) - 65,
                       as(<integer>, k2) - 65,
                       as(<integer>, k3) - 65),
       notches: vector($notch[ri[0]], $notch[ri[1]], $notch[ri[2]]),
       plugboard: pb)
end function;

define method fwd-pass (e :: <enigma>, rotor :: <integer>, idx :: <integer>)
 => (r :: <integer>)
  let contact = mod26(idx + e.offsets[rotor]);
  let out = as(<integer>, $fwd[e.rotors[rotor]][contact]) - 65;
  mod26(out - e.offsets[rotor])
end method;

define method bwd-pass (e :: <enigma>, rotor :: <integer>, idx :: <integer>)
 => (r :: <integer>)
  let contact = mod26(idx + e.offsets[rotor]);
  let out = as(<integer>, $bwd[e.rotors[rotor]][contact]) - 65;
  mod26(out - e.offsets[rotor])
end method;

define method step-rotors (e :: <enigma>) => ()
  if (e.offsets[1] = e.notches[1])
    e.offsets[1] := mod26(e.offsets[1] + 1);
    e.offsets[0] := mod26(e.offsets[0] + 1);
  elseif (e.offsets[2] = e.notches[2])
    e.offsets[1] := mod26(e.offsets[1] + 1);
  end if;
  e.offsets[2] := mod26(e.offsets[2] + 1);
end method;

define method press-key (e :: <enigma>, c :: <character>)
 => (r :: <character>)
  step-rotors(e);
  let idx = e.plugboard[as(<integer>, c) - 65];
  idx := fwd-pass(e, 2, idx);
  idx := fwd-pass(e, 1, idx);
  idx := fwd-pass(e, 0, idx);
  idx := as(<integer>, $ref[idx]) - 65;
  idx := bwd-pass(e, 0, idx);
  idx := bwd-pass(e, 1, idx);
  idx := bwd-pass(e, 2, idx);
  idx := e.plugboard[idx];
  as(<character>, 65 + idx)
end method;

define method encrypt (e :: <enigma>, text :: <string>) => (r :: <string>)
  let result = make(<stretchy-vector>);
  for (c in as-uppercase(text))
    if (c >= 'A' & c <= 'Z')
      add!(result, press-key(e, c))
    end
  end;
  as(<string>, result)
end method;

define function main () => ()
  format-out("Enigma Machine - Dylan Implementation\n");
  let tests = vector(
    vector(#(1,2,3), "AAA", "", "AAAAA", "BDZGO"),
    vector(#(1,2,3), "AAA", "", "HELLOWORLD", "ILBDAAMTAZ"),
    vector(#(1,2,3), "AAA", "", "ATTACKATDAWN", "BZHGNOCRRTCM"),
    vector(#(1,2,3), "MCK", "", "HELLOWORLD", "DLTBBQVPQV"),
    vector(#(3,1,2), "AAA", "", "HELLOWORLD", "KZHDFQYHXT"),
    vector(#(1,2,3), "AAA", "AB-CD-EF", "HELLOWORLD", "IKACBBMTBF"));
  for (t in tests, i from 1)
    let e = make-enigma(t[0][0], t[0][1], t[0][2],
                        t[1][0], t[1][1], t[1][2], plug: t[2]);
    let result = encrypt(e, t[3]);
    let ok = result = t[4];
    format-out("Test %d: %s -> %s %s\n", i, t[3], result,
               if (ok) "[PASS]" else "[FAIL]" end);
  end;
end function;

main();
