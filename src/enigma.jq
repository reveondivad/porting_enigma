# Enigma Cipher in jq
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# Usage: echo '{"rotors":[0,1,2],"key":[0,0,0],"plug":[],"msg":"AAAAA"}' | jq -f enigma.jq

def fwd: [[4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
           [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
           [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]];

def bwd: [[20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
           [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
           [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]];

def ref: [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
def notch: [16,4,21];

def mod26: ((. % 26) + 26) % 26;

def pass_fwd(rotor; offset):
  ((. + offset) | mod26) as $inp |
  (fwd[rotor][$inp] - offset) | mod26;

def pass_bwd(rotor; offset):
  ((. + offset) | mod26) as $inp |
  (bwd[rotor][$inp] - offset) | mod26;

def step_rotors:
  if .o1 == (notch[.r1]) then
    .o1 = ((.o1 + 1) | mod26) | .o0 = ((.o0 + 1) | mod26)
  elif .o2 == (notch[.r2]) then
    .o1 = ((.o1 + 1) | mod26)
  else . end |
  .o2 = ((.o2 + 1) | mod26);

def encrypt_char:
  step_rotors |
  .ch = .pb[.ch] |
  .ch = (.ch | pass_fwd(.r2; .o2)) |
  .ch = (.ch | pass_fwd(.r1; .o1)) |
  .ch = (.ch | pass_fwd(.r0; .o0)) |
  .ch = ref[.ch] |
  .ch = (.ch | pass_bwd(.r0; .o0)) |
  .ch = (.ch | pass_bwd(.r1; .o1)) |
  .ch = (.ch | pass_bwd(.r2; .o2)) |
  .ch = .pb[.ch];

def enigma:
  .rotors as $r | .key as $k | .plug as $plug |
  .msg | explode | map(. - 65) |
  [range(26)] as $identity |
  (reduce $plug[] as $p ($identity; .[$p[0]] = $p[1] | .[$p[1]] = $p[0])) as $pb |
  { r0: $r[0], r1: $r[1], r2: $r[2],
    o0: $k[0], o1: $k[1], o2: $k[2],
    pb: $pb, result: [] } |
  reduce (.[]) as $c (.; .ch = $c | encrypt_char | .result += [.ch]) |
  .result | map(. + 65) | implode;

# Test vectors
{ rotors: [0,1,2], key: [0,0,0], plug: [], msg: "AAAAA" } | "Test 1: " + enigma,
{ rotors: [0,1,2], key: [0,0,0], plug: [], msg: "HELLOWORLD" } | "Test 2: " + enigma,
{ rotors: [0,1,2], key: [0,0,0], plug: [], msg: "ATTACKATDAWN" } | "Test 3: " + enigma,
{ rotors: [0,1,2], key: [12,2,10], plug: [], msg: "HELLOWORLD" } | "Test 4: " + enigma,
{ rotors: [2,0,1], key: [0,0,0], plug: [], msg: "HELLOWORLD" } | "Test 5: " + enigma,
{ rotors: [0,1,2], key: [0,0,0], plug: [[0,1],[2,3],[4,5]], msg: "HELLOWORLD" } | "Test 6: " + enigma
