(* Enigma Cipher - Coq *)
(* Proof assistant / dependently-typed functional programming *)

Require Import List String Ascii Nat.
Require Import Arith.
Import ListNotations.

Definition fwdI := [4;10;12;5;11;6;3;16;21;25;13;19;14;22;24;7;23;20;18;15;0;8;1;17;2;9].
Definition fwdII := [0;9;3;10;18;8;17;20;23;1;11;7;22;19;12;2;16;6;25;13;15;24;5;21;14;4].
Definition fwdIII := [1;3;5;7;9;11;2;15;17;19;23;21;25;13;24;4;8;22;6;0;10;12;20;18;16;14].

Definition bwdI := [20;22;24;6;0;3;5;15;21;25;1;4;2;10;12;19;7;23;18;11;17;8;13;16;14;9].
Definition bwdII := [0;9;15;2;25;22;17;11;5;1;3;10;14;19;24;20;16;6;4;13;7;23;12;8;21;18].
Definition bwdIII := [19;0;6;1;15;2;18;3;16;4;20;5;21;13;25;7;24;8;23;9;22;11;17;10;14;12].

Definition notches := [16; 4; 21].
Definition reflectorB := [24;17;20;7;16;18;11;3;15;23;13;6;14;10;12;8;4;1;5;25;2;22;21;9;0;19].

Definition mod26 (x : nat) : nat := Nat.modulo x 26.

Fixpoint nth_default (l : list nat) (n : nat) : nat :=
  match l, n with
  | x :: _, 0 => x
  | _ :: rest, S n' => nth_default rest n'
  | [], _ => 0
  end.

Definition getFwd (r : nat) : list nat :=
  match r with
  | 0 => fwdI
  | 1 => fwdII
  | _ => fwdIII
  end.

Definition getBwd (r : nat) : list nat :=
  match r with
  | 0 => bwdI
  | 1 => bwdII
  | _ => bwdIII
  end.

Definition getNotch (r : nat) : nat := nth_default notches r.

Definition passFwd (rotor offset ch : nat) : nat :=
  let inp := mod26 (ch + offset) in
  let out := nth_default (getFwd rotor) inp in
  mod26 (out + 26 - offset).

Definition passBwd (rotor offset ch : nat) : nat :=
  let inp := mod26 (ch + offset) in
  let out := nth_default (getBwd rotor) inp in
  mod26 (out + 26 - offset).

(* Plugboard as association list *)
Fixpoint applyPlugboard (pb : list (nat * nat)) (ch : nat) : nat :=
  match pb with
  | [] => ch
  | (a, b) :: rest =>
    if Nat.eqb ch a then b
    else if Nat.eqb ch b then a
    else applyPlugboard rest ch
  end.

Record EnigmaState := mkState {
  r0 : nat; r1 : nat; r2 : nat;
  o0 : nat; o1 : nat; o2 : nat;
  pb : list (nat * nat)
}.

Definition stepAndEncrypt (st : EnigmaState) (ch : nat) : nat * EnigmaState :=
  let mid := Nat.eqb (o1 st) (getNotch (r1 st)) in
  let atn := Nat.eqb (o2 st) (getNotch (r2 st)) in
  let no2 := mod26 (o2 st + 1) in
  let no1 := if orb atn mid then mod26 (o1 st + 1) else o1 st in
  let no0 := if mid then mod26 (o0 st + 1) else o0 st in
  let c := applyPlugboard (pb st) ch in
  let c := passFwd (r2 st) no2 c in
  let c := passFwd (r1 st) no1 c in
  let c := passFwd (r0 st) no0 c in
  let c := nth_default reflectorB c in
  let c := passBwd (r0 st) no0 c in
  let c := passBwd (r1 st) no1 c in
  let c := passBwd (r2 st) no2 c in
  let c := applyPlugboard (pb st) c in
  (c, mkState (r0 st) (r1 st) (r2 st) no0 no1 no2 (pb st)).

Fixpoint encryptChars (st : EnigmaState) (chars : list nat) : list nat :=
  match chars with
  | [] => []
  | ch :: rest =>
    let '(c, st') := stepAndEncrypt st ch in
    c :: encryptChars st' rest
  end.

Definition encrypt (rotors : nat * nat * nat) (key : nat * nat * nat)
                   (pairs : list (nat * nat)) (msg : list nat) : list nat :=
  let '(r0, r1, r2) := rotors in
  let '(k0, k1, k2) := key in
  let st := mkState r0 r1 r2 k0 k1 k2 pairs in
  encryptChars st msg.

(* Test verification via Compute *)
(* AAAAA = [0;0;0;0;0], expected BDZGO = [1;3;25;6;14] *)
Compute (encrypt (0,1,2) (0,0,0) [] [0;0;0;0;0]).
(* Expected: [1; 3; 25; 6; 14] *)

(* HELLOWORLD = [7;4;11;11;14;22;14;17;11;3] *)
Compute (encrypt (0,1,2) (0,0,0) [] [7;4;11;11;14;22;14;17;11;3]).
(* Expected ILBDAAMTAZ = [8;11;1;3;0;0;12;19;0;25] *)

(* Test with key MCK = (12,2,10) *)
Compute (encrypt (0,1,2) (12,2,10) [] [7;4;11;11;14;22;14;17;11;3]).
(* Expected DLTBBQVPQV = [3;11;19;1;1;16;21;15;16;21] *)

(* Test with rotors III-I-II *)
Compute (encrypt (2,0,1) (0,0,0) [] [7;4;11;11;14;22;14;17;11;3]).
(* Expected KZHDFQYHXT = [10;25;7;3;5;16;24;7;23;19] *)

(* Test with plugboard AB-CD-EF *)
Compute (encrypt (0,1,2) (0,0,0) [(0,1);(2,3);(4,5)] [7;4;11;11;14;22;14;17;11;3]).
(* Expected IKACBBMTBF = [8;10;0;2;1;1;12;19;1;5] *)
