NB. ENIGMA CIPHER IN J
NB. Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
NB. J is an array-oriented language descended from APL

NB. Rotor wirings as 0-based index arrays
ROTOR_I    =: 4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9
ROTOR_II   =: 0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4
ROTOR_III  =: 1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14
REFLECTOR  =: 24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19

NB. Notch positions
NOTCH_I  =: 16  NB. Q
NOTCH_II =: 4   NB. E
NOTCH_III=: 21  NB. V

NB. Inverse rotor: for each rotor, find position of each value 0-25
inv =: /:~@i.&26 NB. inverse permutation
ROTOR_I_INV  =: ROTOR_I  i.  i.26
ROTOR_II_INV =: ROTOR_II i.  i.26
ROTOR_III_INV=: ROTOR_III i. i.26

NB. Forward pass: ch -> (rotor{~(ch+offset) mod 26) - offset) mod 26
fwd =: 4 : 0
  'wiring offset' =. y
  ch =. x
  idx =. 26 | ch + offset
  26 | (idx { wiring) - offset
)

NB. Backward pass: ch -> (pos_of((ch+offset)mod26) - offset) mod 26
bwd =: 4 : 0
  'wiring offset' =. y
  ch =. x
  idx =. 26 | ch + offset
  pos =. wiring i. idx
  26 | pos - offset
)

NB. Plugboard: swap pairs
NB. plug is a 26-element array initialized to identity
mkplug =: 3 : 0
  p =. i.26
  pairs =. y
  if. 0 = #pairs do. p return. end.
  for_pair. pairs do.
    a =. {. pair
    b =. {: pair
    p =. b (a}) p
    p =. a (b}) p
  end.
  p
)

NB. Encrypt one character, returns (encrypted_char ; newR ; newM ; newL)
NB. x = character (0-25), y = (rR_wiring;rM_wiring;rL_wiring;rR_inv;rM_inv;rL_inv;oR;oM;oL;notchR;notchM;plug)
encrypt1 =: 4 : 0
  ch =. x
  'wR wM wL wRi wMi wLi oR oM oL nR nM plug' =. y

  NB. Step rotors (before encryption)
  midAtNotch =. oM = nM
  rAtNotch   =. oR = nR
  newR =. 26 | oR + 1
  newM =. 26 | oM + midAtNotch +. rAtNotch
  newL =. 26 | oL + midAtNotch

  NB. Plugboard
  c =. ch { plug

  NB. Forward: R -> M -> L
  c =. c fwd wR;newR
  c =. c fwd wM;newM
  c =. c fwd wL;newL

  NB. Reflector
  c =. c { REFLECTOR

  NB. Backward: L -> M -> R
  c =. c bwd wL;newL
  c =. c bwd wM;newM
  c =. c bwd wR;newR

  NB. Plugboard
  c =. c { plug

  c;newR;newM;newL
)

NB. Encrypt string
NB. y = (rotors;key;plugboard_pairs;plaintext_string)
enigma =: 3 : 0
  'rotors key plugpairs text' =. y

  NB. Select rotor wirings based on rotor numbers (1 2 3)
  wirings =. ROTOR_I;ROTOR_II;ROTOR_III
  invs    =. ROTOR_I_INV;ROTOR_II_INV;ROTOR_III_INV
  notches =. NOTCH_I;NOTCH_II;NOTCH_III

  rL =. > ({. rotors) { wirings
  rM =. > (1{rotors) { wirings
  rR =. > (2{rotors) { wirings
  rLi=. > ({. rotors) { invs
  rMi=. > (1{rotors) { invs
  rRi=. > (2{rotors) { invs
  nM =. > (1{rotors) { notches
  nR =. > (2{rotors) { notches

  NB. Initial offsets from key
  oL =. {. key
  oM =. 1{key
  oR =. 2{key

  plug =. mkplug plugpairs

  NB. Convert text to numbers
  chars =. (a. i. text) - 65

  NB. Encrypt each character
  result =. ''
  for_ch. chars do.
    r =. ch encrypt1 rR;rM;rL;rRi;rMi;rLi;oR;oM;oL;nR;nM;plug
    result =. result , > {. r
    oR =. > 1{r
    oM =. > 2{r
    oL =. > 3{r
  end.

  NB. Convert back to characters
  (65 + result) { a.
)

NB. === TEST VECTORS ===
smoutput 'Enigma J Implementation'
smoutput '======================='

NB. Test 1: Rotors I-II-III, Key AAA, no plugboard: AAAAA -> BDZGO
r1 =. enigma (0 1 2);(0 0 0);(0 2$0);'AAAAA'
smoutput 'Test 1: ' , r1 , ' ' , (r1 -: 'BDZGO') { 'FAIL';'PASS'

NB. Test 2: HELLOWORLD -> ILBDAAMTAZ
r2 =. enigma (0 1 2);(0 0 0);(0 2$0);'HELLOWORLD'
smoutput 'Test 2: ' , r2 , ' ' , (r2 -: 'ILBDAAMTAZ') { 'FAIL';'PASS'

NB. Test 3: ATTACKATDAWN -> BZHGNOCRRTCM
r3 =. enigma (0 1 2);(0 0 0);(0 2$0);'ATTACKATDAWN'
smoutput 'Test 3: ' , r3 , ' ' , (r3 -: 'BZHGNOCRRTCM') { 'FAIL';'PASS'

NB. Test 4: Key MCK -> DLTBBQVPQV
r4 =. enigma (0 1 2);(12 2 10);(0 2$0);'HELLOWORLD'
smoutput 'Test 4: ' , r4 , ' ' , (r4 -: 'DLTBBQVPQV') { 'FAIL';'PASS'

NB. Test 5: Rotors III-I-II -> KZHDFQYHXT
r5 =. enigma (2 0 1);(0 0 0);(0 2$0);'HELLOWORLD'
smoutput 'Test 5: ' , r5 , ' ' , (r5 -: 'KZHDFQYHXT') { 'FAIL';'PASS'

NB. Test 6: Plugboard AB CD EF -> IKACBBMTBF
r6 =. enigma (0 1 2);(0 0 0);(3 2$ 0 1 2 3 4 5);'HELLOWORLD'
smoutput 'Test 6: ' , r6 , ' ' , (r6 -: 'IKACBBMTBF') { 'FAIL';'PASS'
