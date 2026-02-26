// Enigma Machine - Pony Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

primitive Mod26
  fun apply(a: I32): I32 => ((a % 26) + 26) % 26

class Rotor
  let fwd: String
  let bwd: String
  let notch: I32
  var offset: I32

  new create(num: USize, win: U8) =>
    let fwd_arr = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ";"AJDKSIRUXBLHWTMCQGZNPYFVOE";"BDFHJLCPRTXVZNYEIWGAKMUSQO"]
    let bwd_arr = ["UWYGADFPVZBECKMTHXSLRINQOJ";"AJPCZWRLFBDKOTYUQGENHXMIVS";"TAGBPCSDQEUFVNZHYIXJWLRKOM"]
    let notches: Array[I32] val = [16; 4; 21]
    fwd = try fwd_arr(num)? else "" end
    bwd = try bwd_arr(num)? else "" end
    notch = try notches(num)? else 0 end
    offset = (win - 'A').i32()

  fun ref forward(idx: I32): I32 =>
    let contact = Mod26(idx + offset)
    try
      Mod26((fwd(contact.usize())? - 'A').i32() - offset)
    else 0 end

  fun ref backward(idx: I32): I32 =>
    let contact = Mod26(idx + offset)
    try
      Mod26((bwd(contact.usize())? - 'A').i32() - offset)
    else 0 end

  fun ref step_r() => offset = (offset + 1) % 26
  fun at_notch(): Bool => offset == notch

class Enigma
  let left: Rotor
  let middle: Rotor
  let right: Rotor
  let plug: Array[I32]

  new create(rotors: Array[USize] val, key: String, plugboard: Array[String] val) =>
    left = Rotor(try rotors(0)? else 0 end, try key(0)? else 'A' end)
    middle = Rotor(try rotors(1)? else 0 end, try key(1)? else 'A' end)
    right = Rotor(try rotors(2)? else 0 end, try key(2)? else 'A' end)
    plug = Array[I32].init(where f = {(i: USize): I32 => i.i32()}, count = 26)
    for pair in plugboard.values() do
      try
        let a = (pair(0)? - 'A').i32()
        let b = (pair(1)? - 'A').i32()
        plug(a.usize())? = b
        plug(b.usize())? = a
      end
    end

  fun ref step_rotors() =>
    if middle.at_notch() then
      middle.step_r(); left.step_r()
    elseif right.at_notch() then
      middle.step_r()
    end
    right.step_r()

  fun ref press_key(c: U8): U8 =>
    step_rotors()
    let refl = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
    var idx: I32 = (c - 'A').i32()
    try idx = plug(idx.usize())? end
    idx = right.forward(idx)
    idx = middle.forward(idx)
    idx = left.forward(idx)
    try idx = (refl(idx.usize())? - 'A').i32() end
    idx = left.backward(idx)
    idx = middle.backward(idx)
    idx = right.backward(idx)
    try idx = plug(idx.usize())? end
    (idx.u8() + 'A')

  fun ref encrypt(text: String): String =>
    let result = recover String end
    for c in text.upper().values() do
      if (c >= 'A') and (c <= 'Z') then
        result.push(press_key(c))
      end
    end
    consume result

actor Main
  new create(env: Env) =>
    env.out.print("Enigma Machine - Pony Implementation")
    env.out.print("=====================================")

    let tests: Array[(Array[USize] val, String, Array[String] val, String, String)] val = [
      ([as USize: 0;1;2], "AAA", [as String:],             "AAAAA",        "BDZGO")
      ([as USize: 0;1;2], "AAA", [as String:],             "HELLOWORLD",   "ILBDAAMTAZ")
      ([as USize: 0;1;2], "AAA", [as String:],             "ATTACKATDAWN", "BZHGNOCRRTCM")
      ([as USize: 0;1;2], "MCK", [as String:],             "HELLOWORLD",   "DLTBBQVPQV")
      ([as USize: 2;0;1], "AAA", [as String:],             "HELLOWORLD",   "KZHDFQYHXT")
      ([as USize: 0;1;2], "AAA", ["AB";"CD";"EF"],         "HELLOWORLD",   "IKACBBMTBF")
    ]

    var all_pass = true
    for (i, t) in tests.pairs() do
      (let rotors, let key, let plugs, let plain, let expected) = t
      let e = Enigma(rotors, key, plugs)
      let cipher = e.encrypt(plain)
      let ok = cipher == expected
      let status = if ok then "PASS" else "FAIL" end
      env.out.print("  Test " + (i+1).string() + ": " + plain + " -> " + cipher + " [" + status + "]")
      if not ok then
        env.out.print("          Expected " + expected)
        all_pass = false
      end
    end
    if all_pass then env.out.print("\n  ALL 6 TESTS PASSED")
    else env.out.print("\n  SOME TESTS FAILED") end
