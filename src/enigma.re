/* Enigma Cipher - ReasonML (ReScript/BuckleScript)
   Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
   PeopleTec Inc. - Guinness World Record Attempt 2026 */

let fwd = [|
  [|4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9|],
  [|0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4|],
  [|1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14|]
|];

let bwd = [|
  [|20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9|],
  [|0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18|],
  [|19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12|]
|];

let reflector = [|24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19|];
let notch = [|16, 4, 21|];

let mod26 = n => {
  let m = n mod 26;
  m < 0 ? m + 26 : m;
};

type state = {
  rotors: array(int),
  mutable offsets: array(int),
  notches: array(int),
  plugboard: array(int),
};

let makeState = (r1, r2, r3, k1, k2, k3) => {
  let ri = [|r1-1, r2-1, r3-1|];
  {
    rotors: ri,
    offsets: [|k1, k2, k3|],
    notches: [|notch[ri[0]], notch[ri[1]], notch[ri[2]]|],
    plugboard: Array.init(26, i => i),
  };
};

let step = s => {
  if (s.offsets[1] == s.notches[1]) {
    s.offsets[1] = mod26(s.offsets[1] + 1);
    s.offsets[0] = mod26(s.offsets[0] + 1);
  } else if (s.offsets[2] == s.notches[2]) {
    s.offsets[1] = mod26(s.offsets[1] + 1);
  };
  s.offsets[2] = mod26(s.offsets[2] + 1);
};

let fwdPass = (s, rotor, idx) => {
  let contact = mod26(idx + s.offsets[rotor]);
  let out = fwd[s.rotors[rotor]][contact];
  mod26(out - s.offsets[rotor]);
};

let bwdPass = (s, rotor, idx) => {
  let contact = mod26(idx + s.offsets[rotor]);
  let out = bwd[s.rotors[rotor]][contact];
  mod26(out - s.offsets[rotor]);
};

let pressKey = (s, c) => {
  step(s);
  let idx = ref(s.plugboard[Char.code(c) - 65]);
  let idx = fwdPass(s, 2, idx);
  let idx = fwdPass(s, 1, idx);
  let idx = fwdPass(s, 0, idx);
  let idx = reflector[idx];
  let idx = bwdPass(s, 0, idx);
  let idx = bwdPass(s, 1, idx);
  let idx = bwdPass(s, 2, idx);
  let idx = s.plugboard[idx];
  Char.chr(65 + idx);
};

let encrypt = (s, text) => {
  let upper = String.uppercase_ascii(text);
  let buf = Buffer.create(String.length(upper));
  String.iter(c =>
    if (c >= 'A' && c <= 'Z') {
      Buffer.add_char(buf, pressKey(s, c));
    }, upper);
  Buffer.contents(buf);
};

let () = {
  print_endline("Enigma Cipher - ReasonML");
  let tests = [|
    (1,2,3, 0,0,0, "AAAAA", "BDZGO"),
    (1,2,3, 0,0,0, "HELLOWORLD", "ILBDAAMTAZ"),
    (1,2,3, 0,0,0, "ATTACKATDAWN", "BZHGNOCRRTCM"),
    (1,2,3, 12,2,10, "HELLOWORLD", "DLTBBQVPQV"),
    (3,1,2, 0,0,0, "HELLOWORLD", "KZHDFQYHXT"),
  |];
  Array.iteri((i, (r1,r2,r3,k1,k2,k3,input,expected)) => {
    let s = makeState(r1,r2,r3,k1,k2,k3);
    let result = encrypt(s, input);
    let ok = result == expected;
    Printf.printf("Test %d: %s -> %s %s\n", i+1, input, result,
                   ok ? "[PASS]" : "[FAIL]");
  }, tests);
};
