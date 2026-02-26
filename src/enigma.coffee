# Enigma Machine - CoffeeScript Implementation
# Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
# PeopleTec Inc. - Guinness World Record Attempt 2026

FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"]
BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"]
REF = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
NOTCH = [16, 4, 21]

mod26 = (n) -> ((n % 26) + 26) % 26

class Enigma
  constructor: (r1, r2, r3, k1, k2, k3, plugPairs = "") ->
    @rotors = [r1 - 1, r2 - 1, r3 - 1]
    @offsets = [k1.charCodeAt(0) - 65, k2.charCodeAt(0) - 65, k3.charCodeAt(0) - 65]
    @notches = [NOTCH[@rotors[0]], NOTCH[@rotors[1]], NOTCH[@rotors[2]]]
    @plugboard = (i for i in [0...26])
    if plugPairs
      for pair in plugPairs.split("-")
        a = pair.charCodeAt(0) - 65
        b = pair.charCodeAt(1) - 65
        @plugboard[a] = b
        @plugboard[b] = a

  step: ->
    if @offsets[1] == @notches[1]
      @offsets[1] = mod26(@offsets[1] + 1)
      @offsets[0] = mod26(@offsets[0] + 1)
    else if @offsets[2] == @notches[2]
      @offsets[1] = mod26(@offsets[1] + 1)
    @offsets[2] = mod26(@offsets[2] + 1)

  fwdPass: (rotor, idx) ->
    contact = mod26(idx + @offsets[rotor])
    out = FWD[@rotors[rotor]].charCodeAt(contact) - 65
    mod26(out - @offsets[rotor])

  bwdPass: (rotor, idx) ->
    contact = mod26(idx + @offsets[rotor])
    out = BWD[@rotors[rotor]].charCodeAt(contact) - 65
    mod26(out - @offsets[rotor])

  pressKey: (c) ->
    @step()
    idx = @plugboard[c.charCodeAt(0) - 65]
    idx = @fwdPass(2, idx)
    idx = @fwdPass(1, idx)
    idx = @fwdPass(0, idx)
    idx = REF.charCodeAt(idx) - 65
    idx = @bwdPass(0, idx)
    idx = @bwdPass(1, idx)
    idx = @bwdPass(2, idx)
    idx = @plugboard[idx]
    String.fromCharCode(65 + idx)

  encrypt: (text) ->
    (for c in text.toUpperCase() when /[A-Z]/.test(c)
      @pressKey(c)
    ).join("")

# Test harness
console.log "Enigma Machine - CoffeeScript Implementation"
console.log "============================================="

tests = [
  [[1,2,3], "AAA", "", "AAAAA", "BDZGO"]
  [[1,2,3], "AAA", "", "HELLOWORLD", "ILBDAAMTAZ"]
  [[1,2,3], "AAA", "", "ATTACKATDAWN", "BZHGNOCRRTCM"]
  [[1,2,3], "MCK", "", "HELLOWORLD", "DLTBBQVPQV"]
  [[3,1,2], "AAA", "", "HELLOWORLD", "KZHDFQYHXT"]
  [[1,2,3], "AAA", "AB-CD-EF", "HELLOWORLD", "IKACBBMTBF"]
]

pass = 0
for test, i in tests
  [rotors, key, plug, input, expected] = test
  e = new Enigma(rotors[0], rotors[1], rotors[2], key[0], key[1], key[2], plug)
  result = e.encrypt(input)
  ok = result == expected
  pass++ if ok
  console.log "Test #{i+1}: #{input} -> #{result} #{if ok then '[PASS]' else '[FAIL] expected ' + expected}"

console.log "\n#{pass}/6 tests passed"