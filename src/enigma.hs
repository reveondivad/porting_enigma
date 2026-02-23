-- Enigma Machine — Rosetta Code Reference (Haskell)
-- Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
-- PeopleTec Inc. — Guinness World Record Attempt 2026
module Main where
import Data.Char (ord, chr, toUpper, isAlpha)
import Data.List (foldl')

fwdW, bwdW :: [String]
fwdW = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"]
bwdW = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"]

notches :: [Int]
notches = map (\c -> ord c - 65) "QEV"

reflector :: String
reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

idx :: Char -> Int
idx c = ord (toUpper c) - 65

data Enigma = Enigma { eId :: [Int], eOff :: [Int], ePlug :: [Int] }

mkEnigma :: [Int] -> String -> [String] -> Enigma
mkEnigma rotors key plugs = Enigma rotors offs plug
  where offs = map idx key
        plug = foldl' applySwap [0..25] plugs
        applySwap p s = let a = idx (s!!0); b = idx (s!!1)
                        in [if i==a then b else if i==b then a else p!!i | i <- [0..25]]

fwdPass, bwdPass :: Enigma -> Int -> Int -> Int
fwdPass e r i = let c = (i + eOff e !! r) `mod` 26
                    o = idx ((fwdW !! (eId e !! r)) !! c)
                in (o - eOff e !! r + 26) `mod` 26
bwdPass e r i = let c = (i + eOff e !! r) `mod` 26
                    o = idx ((bwdW !! (eId e !! r)) !! c)
                in (o - eOff e !! r + 26) `mod` 26

stepE :: Enigma -> Enigma
stepE e = e { eOff = [o0', o1', o2'] }
  where [o0,o1,o2] = eOff e
        midAtNotch = o1 == notches !! (eId e !! 1)
        rAtNotch   = o2 == notches !! (eId e !! 2)
        (o0', o1', o2')
          | midAtNotch = ((o0+1)`mod`26, (o1+1)`mod`26, (o2+1)`mod`26)
          | rAtNotch   = (o0,            (o1+1)`mod`26, (o2+1)`mod`26)
          | otherwise  = (o0,             o1,           (o2+1)`mod`26)

pressKey :: Enigma -> Char -> (Enigma, Char)
pressKey e0 ch = (e, chr (i7 + 65))
  where e  = stepE e0
        i0 = idx ch
        i1 = ePlug e !! i0
        i2 = fwdPass e 2 i1; i3 = fwdPass e 1 i2; i4 = fwdPass e 0 i3
        i5 = idx (reflector !! i4)
        i6 = bwdPass e 0 i5; i6b = bwdPass e 1 i6; i6c = bwdPass e 2 i6b
        i7 = ePlug e !! i6c

encrypt :: Enigma -> String -> String
encrypt e text = snd $ foldl' go (e, "") $ filter isAlpha $ map toUpper text
  where go (en, acc) c = let (en', c') = pressKey en c in (en', acc ++ [c'])

-- Test vectors
main :: IO ()
main = do
    let tests = [ ([0,1,2],"AAA",[],"AAAAA","BDZGO")
                , ([0,1,2],"AAA",[],"HELLOWORLD","ILBDAAMTAZ")
                , ([0,1,2],"AAA",[],"ATTACKATDAWN","BZHGNOCRRTCM")
                , ([0,1,2],"MCK",[],"HELLOWORLD","DLTBBQVPQV")
                , ([2,0,1],"AAA",[],"HELLOWORLD","KZHDFQYHXT")
                , ([0,1,2],"AAA",["AB","CD","EF"],"HELLOWORLD","IKACBBMTBF")
                ]
    let run (i,(r,k,p,pt,ex)) = do
          let ct = encrypt (mkEnigma r k p) pt
              ok = ct == ex
          putStrLn $ "Test " ++ show i ++ ": " ++ padR 20 pt ++ " -> " ++ padR 15 ct
                     ++ " [" ++ (if ok then "PASS" else "FAIL") ++ "]"
          return ok
    results <- mapM run (zip [1..] tests)
    putStrLn $ if and results then "\nALL 6 TESTS PASSED" else "\nSOME TESTS FAILED"
  where padR n s = s ++ replicate (n - length s) ' '
