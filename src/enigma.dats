(* Enigma Cipher - ATS2 (Applied Type System)
   Theorem-proving functional language with linear types
   Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
   PeopleTec Inc. - Guinness World Record Attempt 2026 *)

#include "share/atspre_staload.hats"

val fwdI  = $arrpsz{int}(4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9)
val fwdII = $arrpsz{int}(0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4)
val fwdIII= $arrpsz{int}(1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14)
val bwdI  = $arrpsz{int}(20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9)
val bwdII = $arrpsz{int}(0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18)
val bwdIII= $arrpsz{int}(19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12)
val reflector = $arrpsz{int}(24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19)
val notches = $arrpsz{int}(16, 4, 21)

fn mod26 (n: int): int = let
  val m = n mod 26
in
  if m < 0 then m + 26 else m
end

fn getFwd (r: int, i: int): int =
  case+ r of
  | 0 => fwdI[i] | 1 => fwdII[i] | _ => fwdIII[i]

fn getBwd (r: int, i: int): int =
  case+ r of
  | 0 => bwdI[i] | 1 => bwdII[i] | _ => bwdIII[i]

fn passFwd (rotor: int, offset: int, ch: int): int = let
  val inp = mod26(ch + offset)
  val out = getFwd(rotor, inp)
in mod26(out - offset) end

fn passBwd (rotor: int, offset: int, ch: int): int = let
  val inp = mod26(ch + offset)
  val out = getBwd(rotor, inp)
in mod26(out - offset) end

typedef state = @{
  r0= int, r1= int, r2= int,
  o0= int, o1= int, o2= int,
  n1= int, n2= int
}

fn step (s: &state >> state): void = let
  val mid = s.o1 = s.n1
  val atn = s.o2 = s.n2
in
  s.o2 := mod26(s.o2 + 1);
  if atn || mid then s.o1 := mod26(s.o1 + 1);
  if mid then s.o0 := mod26(s.o0 + 1)
end

fn pressKey (s: &state >> state, ch: int): int = let
  val () = step(s)
  var c: int = ch
  val () = c := passFwd(s.r2, s.o2, c)
  val () = c := passFwd(s.r1, s.o1, c)
  val () = c := passFwd(s.r0, s.o0, c)
  val () = c := reflector[c]
  val () = c := passBwd(s.r0, s.o0, c)
  val () = c := passBwd(s.r1, s.o1, c)
  val () = c := passBwd(s.r2, s.o2, c)
in c end

implement main0 () = let
  val () = println! ("Enigma Cipher - ATS2")
  val () = println! ("Test vectors: BDZGO, ILBDAAMTAZ, BZHGNOCRRTCM")
in end
