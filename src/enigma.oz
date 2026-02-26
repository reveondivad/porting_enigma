%% Enigma Cipher - Oz (Mozart Programming System)
%% Multi-paradigm: functional, OO, constraint, concurrent

functor
import
   System
   Application
define
   FwdI = {List.toTuple '#' [4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9]}
   FwdII = {List.toTuple '#' [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4]}
   FwdIII = {List.toTuple '#' [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14]}

   BwdI = {List.toTuple '#' [20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9]}
   BwdII = {List.toTuple '#' [0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18]}
   BwdIII = {List.toTuple '#' [19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12]}

   Notches = {List.toTuple '#' [16 4 21]}
   ReflectorB = {List.toTuple '#' [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19]}

   fun {Mod26 X}
      M = X mod 26
   in
      if M < 0 then M + 26 else M end
   end

   fun {GetFwd R I}
      case R of 0 then FwdI.(I+1)
      [] 1 then FwdII.(I+1)
      [] 2 then FwdIII.(I+1)
      end
   end

   fun {GetBwd R I}
      case R of 0 then BwdI.(I+1)
      [] 1 then BwdII.(I+1)
      [] 2 then BwdIII.(I+1)
      end
   end

   fun {PassFwd Rotor Offset Ch}
      Inp = {Mod26 Ch + Offset}
      Out = {GetFwd Rotor Inp}
   in
      {Mod26 Out - Offset}
   end

   fun {PassBwd Rotor Offset Ch}
      Inp = {Mod26 Ch + Offset}
      Out = {GetBwd Rotor Inp}
   in
      {Mod26 Out - Offset}
   end

   fun {MakePlugboard Pairs}
      PB = {NewArray 0 25 0}
   in
      for I in 0..25 do {Put PB I I} end
      for P in Pairs do
         A = P.1
         B = P.2
      in
         {Put PB A B}
         {Put PB B A}
      end
      PB
   end

   fun {Encrypt R0 R1 R2 K0 K1 K2 Pairs Msg}
      PB = {MakePlugboard Pairs}
      O0 = {NewCell K0}
      O1 = {NewCell K1}
      O2 = {NewCell K2}
   in
      {Map {VirtualString.toString Msg}
       fun {$ Ch}
          ChVal = Ch - &A
          Mid = @O1 == Notches.(R1+1)
          Atn = @O2 == Notches.(R2+1)
          C
       in
          O2 := {Mod26 @O2 + 1}
          if Atn orelse Mid then O1 := {Mod26 @O1 + 1} end
          if Mid then O0 := {Mod26 @O0 + 1} end

          C = {Get PB ChVal}
          C = {PassFwd R2 @O2 C}
          C = {PassFwd R1 @O1 C}
          C = {PassFwd R0 @O0 C}
          C = ReflectorB.(C+1)
          C = {PassBwd R0 @O0 C}
          C = {PassBwd R1 @O1 C}
          C = {PassBwd R2 @O2 C}
          C = {Get PB C}
          C + &A
       end}
   end

   proc {RunTest Label Expected Actual}
      Status = if Expected == Actual then "PASS" else "FAIL" end
   in
      {System.showInfo Status#" "#Label#": "#Actual#" (expected "#Expected#")"}
   end

in
   {System.showInfo "Enigma Cipher - Oz"}
   {RunTest "Test 1" "BDZGO"
    {Encrypt 0 1 2 0 0 0 nil "AAAAA"}}
   {RunTest "Test 2" "ILBDAAMTAZ"
    {Encrypt 0 1 2 0 0 0 nil "HELLOWORLD"}}
   {RunTest "Test 3" "BZHGNOCRRTCM"
    {Encrypt 0 1 2 0 0 0 nil "ATTACKATDAWN"}}
   {RunTest "Test 4" "DLTBBQVPQV"
    {Encrypt 0 1 2 12 2 10 nil "HELLOWORLD"}}
   {RunTest "Test 5" "KZHDFQYHXT"
    {Encrypt 2 0 1 0 0 0 nil "HELLOWORLD"}}
   {RunTest "Test 6" "IKACBBMTBF"
    {Encrypt 0 1 2 0 0 0 [0#1 2#3 4#5] "HELLOWORLD"}}
   {Application.exit 0}
end
