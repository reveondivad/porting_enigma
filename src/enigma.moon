-- Enigma Cipher - MoonScript
-- CoffeeScript-like syntax that compiles to Lua

FWD = {
  {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9}
  {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4}
  {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14}
}

BWD = {
  {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9}
  {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18}
  {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12}
}

NOTCH = {16, 4, 21}
REFLECTOR = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19}

mod26 = (x) -> ((x % 26) + 26) % 26

pass_fwd = (rotor, offset, ch) ->
  inp = mod26(ch + offset)
  out = FWD[rotor + 1][inp + 1]
  mod26(out - offset)

pass_bwd = (rotor, offset, ch) ->
  inp = mod26(ch + offset)
  out = BWD[rotor + 1][inp + 1]
  mod26(out - offset)

class Enigma
  new: (r0, r1, r2, k0, k1, k2, pairs) =>
    @r0, @r1, @r2 = r0, r1, r2
    @o0, @o1, @o2 = k0, k1, k2
    @pb = [i for i = 0, 25]
    if pairs
      for pair in *pairs
        @pb[pair[1] + 1] = pair[2]
        @pb[pair[2] + 1] = pair[1]

  step: =>
    mid = @o1 == NOTCH[@r1 + 1]
    atn = @o2 == NOTCH[@r2 + 1]
    @o2 = mod26(@o2 + 1)
    @o1 = mod26(@o1 + 1) if atn or mid
    @o0 = mod26(@o0 + 1) if mid

  encrypt_char: (ch) =>
    @step!
    c = @pb[ch + 1]
    c = pass_fwd @r2, @o2, c
    c = pass_fwd @r1, @o1, c
    c = pass_fwd @r0, @o0, c
    c = REFLECTOR[c + 1]
    c = pass_bwd @r0, @o0, c
    c = pass_bwd @r1, @o1, c
    c = pass_bwd @r2, @o2, c
    @pb[c + 1]

  encrypt: (msg) =>
    result = {}
    for i = 1, #msg
      ch = string.byte(msg, i) - 65
      table.insert result, string.char(@encrypt_char(ch) + 65)
    table.concat result

run_test = (label, expected, rotors, key, pairs, msg) ->
  e = Enigma rotors[1], rotors[2], rotors[3], key[1], key[2], key[3], pairs
  actual = e\encrypt msg
  status = if expected == actual then "PASS" else "FAIL"
  print "#{status} #{label}: #{actual} (expected #{expected})"

print "Enigma Cipher - MoonScript"
run_test "Test 1", "BDZGO", {0,1,2}, {0,0,0}, nil, "AAAAA"
run_test "Test 2", "ILBDAAMTAZ", {0,1,2}, {0,0,0}, nil, "HELLOWORLD"
run_test "Test 3", "BZHGNOCRRTCM", {0,1,2}, {0,0,0}, nil, "ATTACKATDAWN"
run_test "Test 4", "DLTBBQVPQV", {0,1,2}, {12,2,10}, nil, "HELLOWORLD"
run_test "Test 5", "KZHDFQYHXT", {2,0,1}, {0,0,0}, nil, "HELLOWORLD"
run_test "Test 6", "IKACBBMTBF", {0,1,2}, {0,0,0}, {{0,1},{2,3},{4,5}}, "HELLOWORLD"
