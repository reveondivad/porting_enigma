Enigma Cipher in Literate Haskell
==================================

This is a Literate Haskell implementation of the Wehrmacht Enigma I cipher.

> module Enigma where
> import Data.Char (chr, ord, toUpper, isAlpha)

The rotor wirings are stored as lists of integers (0-25):

> rotorFwd :: [[Int]]
> rotorFwd = [ [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
>            , [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
>            , [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14] ]

> rotorBwd :: [[Int]]
> rotorBwd = [ [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
>            , [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
>            , [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12] ]

Reflector B maps each letter to its pair:

> reflector :: [Int]
> reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]

> notches :: [Int]
> notches = [16, 4, 21]

The mod26 function handles negative values correctly:

> mod26 :: Int -> Int
> mod26 n = ((n `mod` 26) + 26) `mod` 26

> rotorPass :: [Int] -> Int -> Int -> Int
> rotorPass wiring c pos = mod26 ((wiring !! mod26 (c + pos)) - pos)

> enigma :: String -> String
> enigma text = snd $ foldl step ([0,0,0], "") (filter isAlpha $ map toUpper text)
>   where
>     step (pos@[p0,p1,p2], acc) ch =
>       let c = ord ch - 65
>           mid = p1 == notches !! 1
>           p2' = if p2 == notches !! 2 then mod26 (p2+1) else p2
>           p1' = if mid || p2' == notches !! 2 then mod26 (p1+1) else p1
>           p2'' = mod26 (p2'+1)
>           pos' = [p0, p1', p2'']
>           c1 = foldr (\i a -> rotorPass (rotorFwd!!i) a (pos'!!i)) c [2,1,0]
>           cr = reflector !! c1
>           c2 = foldl (\a i -> rotorPass (rotorBwd!!i) a (pos'!!i)) cr [0,1,2]
>       in (pos', acc ++ [chr (c2 + 65)])

> main :: IO ()
> main = putStrLn $ enigma "HELLOWORLD"
