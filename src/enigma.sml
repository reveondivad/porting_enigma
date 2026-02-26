(* Enigma Machine - Standard ML Implementation *)
(* Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping) *)
(* PeopleTec Inc. - Guinness World Record Attempt 2026 *)

val fwd = #["EKMFLGDQVZNTOWYHXUSPAIBRCJ",
             "AJDKSIRUXBLHWTMCQGZNPYFVOE",
             "BDFHJLCPRTXVZNYEIWGAKMUSQO"]

val bwd = #["UWYGADFPVZBECKMTHXSLRINQOJ",
             "AJPCZWRLFBDKOTYUQGENHXMIVS",
             "TAGBPCSDQEUFVNZHYIXJWLRKOM"]

val notches = #[16, 4, 21]
val reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

fun mod26 a = ((a mod 26) + 26) mod 26
fun c2i c = Char.ord c - Char.ord #"A"
fun i2c i = Char.chr (i + Char.ord #"A")

type rotor = {fwd: string, bwd: string, notch: int, offset: int ref}

fun makeRotor num win : rotor =
  {fwd = Vector.sub(fwd, num),
   bwd = Vector.sub(bwd, num),
   notch = Vector.sub(notches, num),
   offset = ref (c2i win)}

fun fwdPass (r: rotor) idx =
  let val contact = mod26 (idx + !(#offset r))
      val out = c2i (String.sub(#fwd r, contact))
  in mod26 (out - !(#offset r)) end

fun bwdPass (r: rotor) idx =
  let val contact = mod26 (idx + !(#offset r))
      val out = c2i (String.sub(#bwd r, contact))
  in mod26 (out - !(#offset r)) end

fun stepRotor (r: rotor) = #offset r := ((!(#offset r)) + 1) mod 26
fun atNotch (r: rotor) = !(#offset r) = #notch r

type enigma = {left: rotor, middle: rotor, right: rotor, plug: int array}

fun makeEnigma (rotors: int list) (key: string) (plugboard: string list) : enigma =
  let val plug = Array.tabulate(26, fn i => i)
      val _ = List.app (fn pair =>
        let val a = c2i (String.sub(pair, 0))
            val b = c2i (String.sub(pair, 1))
        in Array.update(plug, a, b); Array.update(plug, b, a) end
      ) plugboard
  in
    {left = makeRotor (List.nth(rotors,0)) (String.sub(key,0)),
     middle = makeRotor (List.nth(rotors,1)) (String.sub(key,1)),
     right = makeRotor (List.nth(rotors,2)) (String.sub(key,2)),
     plug = plug}
  end

fun stepRotors (e: enigma) =
  (if atNotch (#middle e) then (stepRotor (#middle e); stepRotor (#left e))
   else if atNotch (#right e) then stepRotor (#middle e)
   else ();
   stepRotor (#right e))

fun pressKey (e: enigma) c =
  (stepRotors e;
   let val idx = c2i c
       val idx = Array.sub(#plug e, idx)
       val idx = fwdPass (#right e) idx
       val idx = fwdPass (#middle e) idx
       val idx = fwdPass (#left e) idx
       val idx = c2i (String.sub(reflector, idx))
       val idx = bwdPass (#left e) idx
       val idx = bwdPass (#middle e) idx
       val idx = bwdPass (#right e) idx
       val idx = Array.sub(#plug e, idx)
   in i2c idx end)

fun encrypt (e: enigma) text =
  String.implode (List.mapPartial (fn c =>
    if Char.isAlpha c then SOME (pressKey e (Char.toUpper c))
    else NONE
  ) (String.explode text))

(* Test harness *)
val _ = print "Enigma Machine - Standard ML Implementation\n"
val _ = print "=============================================\n"

val tests = [
  ([0,1,2], "AAA", [],              "AAAAA",        "BDZGO"),
  ([0,1,2], "AAA", [],              "HELLOWORLD",   "ILBDAAMTAZ"),
  ([0,1,2], "AAA", [],              "ATTACKATDAWN", "BZHGNOCRRTCM"),
  ([0,1,2], "MCK", [],              "HELLOWORLD",   "DLTBBQVPQV"),
  ([2,0,1], "AAA", [],              "HELLOWORLD",   "KZHDFQYHXT"),
  ([0,1,2], "AAA", ["AB","CD","EF"],"HELLOWORLD",   "IKACBBMTBF")
]

val allPass = ref true
val _ = List.foldl (fn ((rotors, key, plugs, plain, expected), i) =>
  let val e = makeEnigma rotors key plugs
      val cipher = encrypt e plain
      val ok = cipher = expected
      val status = if ok then "PASS" else "FAIL"
  in
    print ("  Test " ^ Int.toString i ^ ": " ^ plain ^ " -> " ^ cipher ^ " [" ^ status ^ "]\n");
    if not ok then (print ("          Expected " ^ expected ^ "\n"); allPass := false) else ();
    i + 1
  end
) 1 tests

val _ = print (if !allPass then "\n  ALL 6 TESTS PASSED\n" else "\n  SOME TESTS FAILED\n")
