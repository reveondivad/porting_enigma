--------------------------- MODULE Enigma ---------------------------
(* Enigma Cipher - TLA+ (Temporal Logic of Actions)
   Formal specification language by Leslie Lamport
   Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
   PeopleTec Inc. - Guinness World Record Attempt 2026 *)

EXTENDS Integers, Sequences, TLC

CONSTANTS
  FwdI, FwdII, FwdIII, BwdI, BwdII, BwdIII, ReflB, Notches

VARIABLES
  rotors, offsets, input, output, idx

vars == <<rotors, offsets, input, output, idx>>

Mod26(n) == ((n % 26) + 26) % 26

GetFwd(r, i) == CASE r = 0 -> FwdI[i+1]
                  [] r = 1 -> FwdII[i+1]
                  [] OTHER -> FwdIII[i+1]

GetBwd(r, i) == CASE r = 0 -> BwdI[i+1]
                  [] r = 1 -> BwdII[i+1]
                  [] OTHER -> BwdIII[i+1]

PassFwd(rotor, offset, ch) ==
  LET inp == Mod26(ch + offset)
      out == GetFwd(rotor, inp)
  IN  Mod26(out - offset)

PassBwd(rotor, offset, ch) ==
  LET inp == Mod26(ch + offset)
      out == GetBwd(rotor, inp)
  IN  Mod26(out - offset)

Step(o) ==
  LET mid == o[2] = Notches[rotors[2]+1]
      atn == o[3] = Notches[rotors[3]+1]
      newO3 == Mod26(o[3] + 1)
      newO2 == IF mid \/ atn THEN Mod26(o[2] + 1) ELSE o[2]
      newO1 == IF mid THEN Mod26(o[1] + 1) ELSE o[1]
  IN <<newO1, newO2, newO3>>

Encrypt(o, ch) ==
  LET no == Step(o)
      c1 == PassFwd(rotors[3], no[3], ch)
      c2 == PassFwd(rotors[2], no[2], c1)
      c3 == PassFwd(rotors[1], no[1], c2)
      c4 == ReflB[c3+1]
      c5 == PassBwd(rotors[1], no[1], c4)
      c6 == PassBwd(rotors[2], no[2], c5)
      c7 == PassBwd(rotors[3], no[3], c6)
  IN <<c7, no>>

Init ==
  /\ rotors = <<0, 1, 2>>
  /\ offsets = <<0, 0, 0>>
  /\ input = <<0, 0, 0, 0, 0>>  \* AAAAA
  /\ output = <<>>
  /\ idx = 1

Next ==
  /\ idx <= Len(input)
  /\ LET result == Encrypt(offsets, input[idx])
     IN /\ output' = Append(output, result[1])
        /\ offsets' = result[2]
        /\ idx' = idx + 1
        /\ UNCHANGED <<rotors, input>>

Spec == Init /\ [][Next]_vars

(* Expected: output = <<1, 3, 25, 6, 14>> = BDZGO *)
Invariant == idx > Len(input) => output = <<1, 3, 25, 6, 14>>

=============================================================================
