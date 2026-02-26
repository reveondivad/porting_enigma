-- Enigma Cipher - Idris 2
-- Dependently-typed functional language
-- Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
-- PeopleTec Inc. - Guinness World Record Attempt 2026

module Enigma

import Data.Vect
import Data.String

fwdI : Vect 26 Int
fwdI = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]

fwdII : Vect 26 Int
fwdII = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]

fwdIII : Vect 26 Int
fwdIII = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]

bwdI : Vect 26 Int
bwdI = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]

bwdII : Vect 26 Int
bwdII = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]

bwdIII : Vect 26 Int
bwdIII = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]

reflector : Vect 26 Int
reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]

notches : Vect 3 Int
notches = [16, 4, 21]

mod26 : Int -> Int
mod26 n = let m = mod n 26 in if m < 0 then m + 26 else m

indexV : Vect 26 Int -> Int -> Int
indexV v i = case natToFin (cast (mod26 i)) 26 of
               Just f  => index f v
               Nothing => 0

getFwd : Int -> Int -> Int
getFwd 0 i = indexV fwdI i
getFwd 1 i = indexV fwdII i
getFwd _ i = indexV fwdIII i

getBwd : Int -> Int -> Int
getBwd 0 i = indexV bwdI i
getBwd 1 i = indexV bwdII i
getBwd _ i = indexV bwdIII i

passFwd : Int -> Int -> Int -> Int
passFwd rotor offset ch =
  let inp = mod26 (ch + offset)
      out = getFwd rotor inp
  in mod26 (out - offset)

passBwd : Int -> Int -> Int -> Int
passBwd rotor offset ch =
  let inp = mod26 (ch + offset)
      out = getBwd rotor inp
  in mod26 (out - offset)

record State where
  constructor MkState
  r0, r1, r2 : Int
  o0, o1, o2 : Int
  n1, n2 : Int

step : State -> State
step s =
  let mid = s.o1 == s.n1
      atn = s.o2 == s.n2
      newO2 = mod26 (s.o2 + 1)
      newO1 = if mid || atn then mod26 (s.o1 + 1) else s.o1
      newO0 = if mid then mod26 (s.o0 + 1) else s.o0
  in { s | o0 := newO0, o1 := newO1, o2 := newO2 }

pressKey : State -> Int -> (Int, State)
pressKey s ch =
  let ns = step s
      c = passFwd ns.r2 ns.o2 ch
      c2 = passFwd ns.r1 ns.o1 c
      c3 = passFwd ns.r0 ns.o0 c2
      c4 = indexV reflector c3
      c5 = passBwd ns.r0 ns.o0 c4
      c6 = passBwd ns.r1 ns.o1 c5
      c7 = passBwd ns.r2 ns.o2 c6
  in (c7, ns)

encryptLoop : State -> List Int -> List Int -> (List Int, State)
encryptLoop s [] acc = (reverse acc, s)
encryptLoop s (ch :: rest) acc =
  let (enc, ns) = pressKey s ch
  in encryptLoop ns rest (enc :: acc)

encrypt : Int -> Int -> Int -> Int -> Int -> Int -> String -> String
encrypt r0 r1 r2 k0 k1 k2 msg =
  let s = MkState r0 r1 r2 k0 k1 k2
            (indexV notches r1) (indexV notches r2)
      chars = map (\c => cast c - 65) (unpack msg)
      (result, _) = encryptLoop s chars []
  in pack (map (\i => cast (i + 65)) result)

main : IO ()
main = do
  putStrLn "Enigma Cipher - Idris 2"
  putStrLn $ "Test 1: " ++ encrypt 0 1 2 0 0 0 "AAAAA"
  putStrLn $ "Test 2: " ++ encrypt 0 1 2 0 0 0 "HELLOWORLD"
  putStrLn $ "Test 3: " ++ encrypt 0 1 2 0 0 0 "ATTACKATDAWN"
  putStrLn $ "Test 4: " ++ encrypt 0 1 2 12 2 10 "HELLOWORLD"
  putStrLn $ "Test 5: " ++ encrypt 2 0 1 0 0 0 "HELLOWORLD"
