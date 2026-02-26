(* Enigma Machine - OCaml Implementation *)
(* Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping) *)
(* PeopleTec Inc. - Guinness World Record Attempt 2026 *)

let fwd = [| "EKMFLGDQVZNTOWYHXUSPAIBRCJ";
             "AJDKSIRUXBLHWTMCQGZNPYFVOE";
             "BDFHJLCPRTXVZNYEIWGAKMUSQO" |]

let bwd = [| "UWYGADFPVZBECKMTHXSLRINQOJ";
             "AJPCZWRLFBDKOTYUQGENHXMIVS";
             "TAGBPCSDQEUFVNZHYIXJWLRKOM" |]

let notch = [| 16; 4; 21 |]  (* Q=16, E=4, V=21 *)
let refl = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

let mod26 a = ((a mod 26) + 26) mod 26
let c2i c = Char.code c - Char.code 'A'
let i2c i = Char.chr (i + Char.code 'A')

type rotor = { fwd: string; bwd: string; notch: int; mutable offset: int }

let make_rotor num win =
  { fwd = fwd.(num); bwd = bwd.(num);
    notch = notch.(num); offset = c2i win }

let fwd_pass r idx =
  let contact = mod26 (idx + r.offset) in
  mod26 (c2i r.fwd.[contact] - r.offset)

let bwd_pass r idx =
  let contact = mod26 (idx + r.offset) in
  mod26 (c2i r.bwd.[contact] - r.offset)

let step r = r.offset <- (r.offset + 1) mod 26

type enigma = {
  left: rotor; middle: rotor; right: rotor;
  plug: int array
}

let make_enigma (r1, r2, r3) key plugboard =
  let plug = Array.init 26 Fun.id in
  List.iter (fun pair ->
    let a = c2i pair.[0] and b = c2i pair.[1] in
    plug.(a) <- b; plug.(b) <- a
  ) plugboard;
  { left = make_rotor r1 key.[0];
    middle = make_rotor r2 key.[1];
    right = make_rotor r3 key.[2];
    plug }

let step_rotors e =
  if e.middle.offset = e.middle.notch then
    (step e.middle; step e.left)
  else if e.right.offset = e.right.notch then
    step e.middle;
  step e.right

let press_key e c =
  step_rotors e;
  let idx = ref (c2i c) in
  idx := e.plug.(!idx);
  idx := fwd_pass e.right !idx;
  idx := fwd_pass e.middle !idx;
  idx := fwd_pass e.left !idx;
  idx := c2i refl.[!idx];
  idx := bwd_pass e.left !idx;
  idx := bwd_pass e.middle !idx;
  idx := bwd_pass e.right !idx;
  idx := e.plug.(!idx);
  i2c !idx

let encrypt e text =
  let buf = Buffer.create 32 in
  String.iter (fun c ->
    let c = Char.uppercase_ascii c in
    if c >= 'A' && c <= 'Z' then
      Buffer.add_char buf (press_key e c)
  ) text;
  Buffer.contents buf

let () =
  Printf.printf "Enigma Machine - OCaml Implementation\n";
  Printf.printf "======================================\n";
  let tests = [
    ((0,1,2), "AAA", [],              "AAAAA",        "BDZGO");
    ((0,1,2), "AAA", [],              "HELLOWORLD",   "ILBDAAMTAZ");
    ((0,1,2), "AAA", [],              "ATTACKATDAWN", "BZHGNOCRRTCM");
    ((0,1,2), "MCK", [],              "HELLOWORLD",   "DLTBBQVPQV");
    ((2,0,1), "AAA", [],              "HELLOWORLD",   "KZHDFQYHXT");
    ((0,1,2), "AAA", ["AB";"CD";"EF"],"HELLOWORLD",   "IKACBBMTBF");
  ] in
  let all_pass = ref true in
  List.iteri (fun i (rotors, key, plugs, plain, expected) ->
    let e = make_enigma rotors key plugs in
    let cipher = encrypt e plain in
    let ok = cipher = expected in
    let status = if ok then "PASS" else "FAIL" in
    Printf.printf "  Test %d: %-20s -> %-15s [%s]\n" (i+1) plain cipher status;
    if not ok then (
      Printf.printf "          Expected %s, got %s\n" expected cipher;
      all_pass := false
    )
  ) tests;
  Printf.printf "\n  %s\n"
    (if !all_pass then "ALL 6 TESTS PASSED" else "SOME TESTS FAILED")
