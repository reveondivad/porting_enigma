-- Enigma Cipher - Curry
-- Functional logic programming language (Haskell + logic)

module Enigma where

fwdI, fwdII, fwdIII :: [Int]
fwdI   = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
fwdII  = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
fwdIII = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]

bwdI, bwdII, bwdIII :: [Int]
bwdI   = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
bwdII  = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
bwdIII = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]

notches :: [Int]
notches = [16, 4, 21]

reflectorB :: [Int]
reflectorB = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]

mod26 :: Int -> Int
mod26 x = (x `mod` 26 + 26) `mod` 26

at :: [Int] -> Int -> Int
at xs i = xs !! i

getFwd :: Int -> [Int]
getFwd 0 = fwdI
getFwd 1 = fwdII
getFwd _ = fwdIII

getBwd :: Int -> [Int]
getBwd 0 = bwdI
getBwd 1 = bwdII
getBwd _ = bwdIII

passFwd :: Int -> Int -> Int -> Int
passFwd rotor offset ch =
  let inp = mod26 (ch + offset)
      out = at (getFwd rotor) inp
  in mod26 (out - offset)

passBwd :: Int -> Int -> Int -> Int
passBwd rotor offset ch =
  let inp = mod26 (ch + offset)
      out = at (getBwd rotor) inp
  in mod26 (out - offset)

makePlugboard :: [(Int,Int)] -> [Int]
makePlugboard pairs = foldl applyPair [0..25] pairs
  where applyPair pb (a,b) = [if i==a then b else if i==b then a else pb!!i | i <- [0..25]]

data State = St Int Int Int Int Int Int [Int]

stepAndEncrypt :: State -> Int -> (Int, State)
stepAndEncrypt (St r0 r1 r2 o0 o1 o2 pb) ch =
  let mid = o1 == at notches r1
      atn = o2 == at notches r2
      no2 = mod26 (o2 + 1)
      no1 = if atn || mid then mod26 (o1 + 1) else o1
      no0 = if mid then mod26 (o0 + 1) else o0
      c = at pb ch
      c1 = passFwd r2 no2 c
      c2 = passFwd r1 no1 c1
      c3 = passFwd r0 no0 c2
      c4 = at reflectorB c3
      c5 = passBwd r0 no0 c4
      c6 = passBwd r1 no1 c5
      c7 = passBwd r2 no2 c6
      c8 = at pb c7
  in (c8, St r0 r1 r2 no0 no1 no2 pb)

encryptChars :: State -> [Int] -> [Int]
encryptChars _ [] = []
encryptChars st (ch:rest) =
  let (c, st') = stepAndEncrypt st ch
  in c : encryptChars st' rest

encrypt :: (Int,Int,Int) -> (Int,Int,Int) -> [(Int,Int)] -> String -> String
encrypt (r0,r1,r2) (k0,k1,k2) pairs msg =
  let pb = makePlugboard pairs
      st = St r0 r1 r2 k0 k1 k2 pb
      chars = map (\c -> ord c - ord 'A') msg
      result = encryptChars st chars
  in map (\i -> chr (i + ord 'A')) result

runTest :: String -> String -> String -> IO ()
runTest label expected actual = do
  let status = if expected == actual then "PASS" else "FAIL"
  putStrLn (status ++ " " ++ label ++ ": " ++ actual ++ " (expected " ++ expected ++ ")")

main :: IO ()
main = do
  putStrLn "Enigma Cipher - Curry"
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
