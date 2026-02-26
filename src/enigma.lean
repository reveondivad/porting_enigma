-- Enigma Cipher - Lean 4
-- Theorem prover and functional programming language

def fwdI : Array Nat := #[4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
def fwdII : Array Nat := #[0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
def fwdIII : Array Nat := #[1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]

def bwdI : Array Nat := #[20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
def bwdII : Array Nat := #[0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
def bwdIII : Array Nat := #[19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]

def notches : Array Nat := #[16, 4, 21]
def reflectorB : Array Nat := #[24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]

def mod26 (x : Int) : Nat :=
  let m := x % 26
  (if m < 0 then m + 26 else m).toNat

def getFwd (r : Nat) : Array Nat :=
  match r with
  | 0 => fwdI
  | 1 => fwdII
  | _ => fwdIII

def getBwd (r : Nat) : Array Nat :=
  match r with
  | 0 => bwdI
  | 1 => bwdII
  | _ => bwdIII

def getNotch (r : Nat) : Nat := notches[r]!

def passFwd (rotor offset ch : Nat) : Nat :=
  let inp := mod26 ((ch : Int) + (offset : Int))
  let out := (getFwd rotor)[inp]!
  mod26 ((out : Int) - (offset : Int))

def passBwd (rotor offset ch : Nat) : Nat :=
  let inp := mod26 ((ch : Int) + (offset : Int))
  let out := (getBwd rotor)[inp]!
  mod26 ((out : Int) - (offset : Int))

def makePlugboard (pairs : List (Nat × Nat)) : Array Nat :=
  let base := Array.range 26
  pairs.foldl (fun pb (a, b) =>
    let pb := pb.set! a b
    pb.set! b a
  ) base

structure EnigmaState where
  r0 : Nat; r1 : Nat; r2 : Nat
  o0 : Nat; o1 : Nat; o2 : Nat
  pb : Array Nat

def encryptChar (st : EnigmaState) (ch : Nat) : Nat × EnigmaState :=
  let mid := st.o1 == getNotch st.r1
  let atn := st.o2 == getNotch st.r2
  let no2 := mod26 ((st.o2 : Int) + 1)
  let no1 := if atn || mid then mod26 ((st.o1 : Int) + 1) else st.o1
  let no0 := if mid then mod26 ((st.o0 : Int) + 1) else st.o0
  let c := st.pb[ch]!
  let c := passFwd st.r2 no2 c
  let c := passFwd st.r1 no1 c
  let c := passFwd st.r0 no0 c
  let c := reflectorB[c]!
  let c := passBwd st.r0 no0 c
  let c := passBwd st.r1 no1 c
  let c := passBwd st.r2 no2 c
  let c := st.pb[c]!
  (c, { st with o0 := no0, o1 := no1, o2 := no2 })

def encryptChars : EnigmaState → List Nat → List Nat
  | _, [] => []
  | st, ch :: rest =>
    let (c, st') := encryptChar st ch
    c :: encryptChars st' rest

def encrypt (rotors : Nat × Nat × Nat) (key : Nat × Nat × Nat)
            (pairs : List (Nat × Nat)) (msg : String) : String :=
  let (r0, r1, r2) := rotors
  let (k0, k1, k2) := key
  let pb := makePlugboard pairs
  let st : EnigmaState := ⟨r0, r1, r2, k0, k1, k2, pb⟩
  let chars := msg.toList.map (fun c => c.toNat - 'A'.toNat)
  let result := encryptChars st chars
  String.mk (result.map (fun i => Char.ofNat (i + 'A'.toNat)))

def runTest (label expected actual : String) : IO Unit := do
  let status := if expected == actual then "PASS" else "FAIL"
  IO.println s!"{status} {label}: {actual} (expected {expected})"

def main : IO Unit := do
  IO.println "Enigma Cipher - Lean 4"
  runTest "Test 1" "BDZGO"
    (encrypt (0,1,2) (0,0,0) [] "AAAAA")
  runTest "Test 2" "ILBDAAMTAZ"
    (encrypt (0,1,2) (0,0,0) [] "HELLOWORLD")
  runTest "Test 3" "BZHGNOCRRTCM"
    (encrypt (0,1,2) (0,0,0) [] "ATTACKATDAWN")
  runTest "Test 4" "DLTBBQVPQV"
    (encrypt (0,1,2) (12,2,10) [] "HELLOWORLD")
  runTest "Test 5" "KZHDFQYHXT"
    (encrypt (2,0,1) (0,0,0) [] "HELLOWORLD")
  runTest "Test 6" "IKACBBMTBF"
    (encrypt (0,1,2) (0,0,0) [(0,1),(2,3),(4,5)] "HELLOWORLD")
