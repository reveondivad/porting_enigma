(* Enigma Machine - Wolfram/Mathematica Implementation *)
(* Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping) *)
(* PeopleTec Inc. - Guinness World Record Attempt 2026 *)

fwdWirings = {"EKMFLGDQVZNTOWYHXUSPAIBRCJ",
               "AJDKSIRUXBLHWTMCQGZNPYFVOE",
               "BDFHJLCPRTXVZNYEIWGAKMUSQO"};
bwdWirings = {"UWYGADFPVZBECKMTHXSLRINQOJ",
               "AJPCZWRLFBDKOTYUQGENHXMIVS",
               "TAGBPCSDQEUFVNZHYIXJWLRKOM"};
notchPos = {16, 4, 21};
reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

mod26[a_] := Mod[a, 26]
c2i[c_] := ToCharacterCode[c][[1]] - 65
i2c[i_] := FromCharacterCode[i + 65]

makeEnigma[rotors_, key_, plugboard_] := Module[{e},
  e = <|
    "lFwd" -> fwdWirings[[rotors[[1]]+1]],
    "lBwd" -> bwdWirings[[rotors[[1]]+1]],
    "lNotch" -> notchPos[[rotors[[1]]+1]],
    "lOff" -> c2i[StringPart[key, 1]],
    "mFwd" -> fwdWirings[[rotors[[2]]+1]],
    "mBwd" -> bwdWirings[[rotors[[2]]+1]],
    "mNotch" -> notchPos[[rotors[[2]]+1]],
    "mOff" -> c2i[StringPart[key, 2]],
    "rFwd" -> fwdWirings[[rotors[[3]]+1]],
    "rBwd" -> bwdWirings[[rotors[[3]]+1]],
    "rNotch" -> notchPos[[rotors[[3]]+1]],
    "rOff" -> c2i[StringPart[key, 3]],
    "plug" -> Range[0, 25]
  |>;
  Do[
    With[{a = c2i[StringPart[pair, 1]], b = c2i[StringPart[pair, 2]]},
      e["plug"] = ReplacePart[e["plug"], {a+1 -> b, b+1 -> a}]
    ], {pair, plugboard}
  ];
  e
]

fwdPass[wiring_, off_, idx_] := Module[{contact},
  contact = mod26[idx + off];
  mod26[c2i[StringPart[wiring, contact + 1]] - off]
]

bwdPass[wiring_, off_, idx_] := Module[{contact},
  contact = mod26[idx + off];
  mod26[c2i[StringPart[wiring, contact + 1]] - off]
]

stepRotors[e_] := Module[{state = e},
  If[state["mOff"] == state["mNotch"],
    state["mOff"] = Mod[state["mOff"] + 1, 26];
    state["lOff"] = Mod[state["lOff"] + 1, 26],
    If[state["rOff"] == state["rNotch"],
      state["mOff"] = Mod[state["mOff"] + 1, 26]
    ]
  ];
  state["rOff"] = Mod[state["rOff"] + 1, 26];
  state
]

pressKey[e_, c_] := Module[{state, idx},
  state = stepRotors[e];
  idx = c2i[c];
  idx = state["plug"][[idx + 1]];
  idx = fwdPass[state["rFwd"], state["rOff"], idx];
  idx = fwdPass[state["mFwd"], state["mOff"], idx];
  idx = fwdPass[state["lFwd"], state["lOff"], idx];
  idx = c2i[StringPart[reflector, idx + 1]];
  idx = bwdPass[state["lBwd"], state["lOff"], idx];
  idx = bwdPass[state["mBwd"], state["mOff"], idx];
  idx = bwdPass[state["rBwd"], state["rOff"], idx];
  idx = state["plug"][[idx + 1]];
  {state, i2c[idx]}
]

encrypt[e_, text_] := Module[{state = e, result = "", chars, ch},
  chars = Characters[ToUpperCase[text]];
  chars = Select[chars, LetterQ];
  Do[
    {state, ch} = pressKey[state, c];
    result = result <> ch,
    {c, chars}
  ];
  result
]

(* Test harness *)
Print["Enigma Machine - Wolfram/Mathematica Implementation"];
Print["===================================================="];

tests = {
  {{0,1,2}, "AAA", {},              "AAAAA",        "BDZGO"},
  {{0,1,2}, "AAA", {},              "HELLOWORLD",   "ILBDAAMTAZ"},
  {{0,1,2}, "AAA", {},              "ATTACKATDAWN", "BZHGNOCRRTCM"},
  {{0,1,2}, "MCK", {},              "HELLOWORLD",   "DLTBBQVPQV"},
  {{2,0,1}, "AAA", {},              "HELLOWORLD",   "KZHDFQYHXT"},
  {{0,1,2}, "AAA", {"AB","CD","EF"},"HELLOWORLD",   "IKACBBMTBF"}
};

allPass = True;
Do[
  With[{t = tests[[i]]},
    Module[{e, cipher, ok, status},
      e = makeEnigma[t[[1]], t[[2]], t[[3]]];
      cipher = encrypt[e, t[[4]]];
      ok = (cipher === t[[5]]);
      status = If[ok, "PASS", "FAIL"];
      Print["  Test ", i, ": ", t[[4]], " -> ", cipher, " [", status, "]"];
      If[!ok, Print["          Expected ", t[[5]]]; allPass = False];
    ]
  ],
  {i, Length[tests]}
];
Print[If[allPass, "\n  ALL 6 TESTS PASSED", "\n  SOME TESTS FAILED"]];
