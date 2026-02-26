-- Enigma Cipher - Idris 2
-- Dependently typed functional programming language

module Enigma

import Data.Vect
import Data.String
import Data.List

-- Rotor wirings (forward)
fwdI : List Int
fwdI = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]

fwdII : List Int
fwdII = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]

fwdIII : List Int
fwdIII = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]

-- Rotor wirings (backward)
bwdI : List Int
bwdI = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]

bwdII : List Int
bwdII = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]

bwdIII : List Int
bwdIII = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]

-- Notch positions
notchI : Int
notchI = 16  -- Q

notchII : Int
notchII = 4  -- E

notchIII : Int
notchIII = 21  -- V

-- Reflector B
reflectorB : List Int
reflectorB = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]

record EnigmaState where
  constructor MkEnigma
  rotorOrder : (Int, Int, Int)  -- indices into rotor arrays
  offsets : (Int, Int, Int)
  plugboard : List Int

getFwd : Int -> List Int
getFwd 0 = fwdI
getFwd 1 = fwdII
getFwd 2 = fwdIII
getFwd _ = fwdI

getBwd : Int -> List Int
getBwd 0 = bwdI
getBwd 1 = bwdII
getBwd 2 = bwdIII
getBwd _ = bwdI

getNotch : Int -> Int
getNotch 0 = notchI
getNotch 1 = notchII
getNotch 2 = notchIII
getNotch _ = notchI

listIndex : List Int -> Int -> Int
listIndex xs i = case drop (cast (the Int i)) xs of
                   (x :: _) => x
                   [] => 0

mod26 : Int -> Int
mod26 x = mod ((mod x 26) + 26) 26

stepRotors : (Int, Int, Int) -> (Int, Int, Int) -> ((Int, Int, Int))
stepRotors (r0, r1, r2) (o0, o1, o2) =
  let mid = o1 == getNotch r1
      o2' = mod26 (o2 + 1)
      o1' = if o2 == getNotch r2 || mid then mod26 (o1 + 1) else o1
      o0' = if mid then mod26 (o0 + 1) else o0
  in (o0', o1', o2')

passForward : Int -> Int -> Int -> Int
passForward rotor offset ch =
  let inp = mod26 (ch + offset)
      out = listIndex (getFwd rotor) inp
  in mod26 (out - offset)

passBackward : Int -> Int -> Int -> Int
passBackward rotor offset ch =
  let inp = mod26 (ch + offset)
      out = listIndex (getBwd rotor) inp
  in mod26 (out - offset)

encryptChar : EnigmaState -> (Int, EnigmaState)  -> Int -> (Int, EnigmaState)
encryptChar _ (_, st) ch =
  let (r0, r1, r2) = rotorOrder st
      (o0, o1, o2) = stepRotors (r0, r1, r2) (offsets st)
      st' = { offsets := (o0, o1, o2) } st
      c0 = listIndex (plugboard st') ch
      c1 = passForward r2 o2 c0
      c2 = passForward r1 o1 c1
      c3 = passForward r0 o0 c2
      c4 = listIndex reflectorB c3
      c5 = passBackward r0 o0 c4
      c6 = passBackward r1 o1 c5
      c7 = passBackward r2 o2 c6
      c8 = listIndex (plugboard st') c7
  in (c8, st')

mkPlugboard : List (Int, Int) -> List Int
mkPlugboard pairs =
  let base = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
      applyPair : List Int -> (Int, Int) -> List Int
      applyPair pb (a, b) = map (\(i, v) => if i == a then b else if i == b then a else v)
                                (zip [0..25] pb)
  in foldl applyPair base pairs

encrypt : EnigmaState -> String -> String
encrypt st msg =
  let chars = map (\c => ord c - ord 'A') (unpack msg)
      (result, _) = foldl (encryptChar st) (0, st) chars
  in pack (map (\i => chr (i + ord 'A')) (reverse result))

-- simplified: direct character-by-character encryption
encryptAll : EnigmaState -> List Int -> List Int -> EnigmaState -> (List Int, EnigmaState)
encryptAll st [] acc st' = (reverse acc, st')
encryptAll st (ch :: rest) acc st' =
  let (r0, r1, r2) = rotorOrder st'
      (o0, o1, o2) = stepRotors (r0, r1, r2) (offsets st')
      st'' = { offsets := (o0, o1, o2) } st'
      c0 = listIndex (plugboard st'') ch
      c1 = passForward r2 o2 c0
      c2 = passForward r1 o1 c1
      c3 = passForward r0 o0 c2
      c4 = listIndex reflectorB c3
      c5 = passBackward r0 o0 c4
      c6 = passBackward r1 o1 c5
      c7 = passBackward r2 o2 c6
      c8 = listIndex (plugboard st'') c7
  in encryptAll st rest (c8 :: acc) st''

doEncrypt : (Int,Int,Int) -> (Int,Int,Int) -> List (Int,Int) -> String -> String
doEncrypt rotors key pairs msg =
  let pb = mkPlugboard pairs
      st = MkEnigma rotors key pb
      chars = map (\c => ord c - ord 'A') (unpack msg)
      (result, _) = encryptAll st chars [] st
  in pack (map (\i => chr (i + ord 'A')) result)

test : String -> String -> String -> IO ()
test label expected actual =
  let status = if expected == actual then "PASS" else "FAIL"
  in putStrLn $ status ++ " " ++ label ++ ": " ++ actual ++ " (expected " ++ expected ++ ")"

main : IO ()
main = do
  putStrLn "Enigma Cipher - Idris 2"
  test "Test 1" "BDZGO"
       (doEncrypt (0,1,2) (0,0,0) [] "AAAAA")
  test "Test 2" "ILBDAAMTAZ"
       (doEncrypt (0,1,2) (0,0,0) [] "HELLOWORLD")
  test "Test 3" "BZHGNOCRRTCM"
       (doEncrypt (0,1,2) (0,0,0) [] "ATTACKATDAWN")
  test "Test 4" "DLTBBQVPQV"
       (doEncrypt (0,1,2) (12,2,10) [] "HELLOWORLD")
  test "Test 5" "KZHDFQYHXT"
       (doEncrypt (2,0,1) (0,0,0) [] "HELLOWORLD")
  test "Test 6" "IKACBBMTBF"
       (doEncrypt (0,1,2) (0,0,0) [(0,1),(2,3),(4,5)] "HELLOWORLD")