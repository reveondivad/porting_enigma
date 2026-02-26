module Enigma

// Enigma Cipher - Clean
// Pure functional language with uniqueness typing

import StdEnv

fwdI  :== {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9}
fwdII :== {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4}
fwdIII:== {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14}

bwdI  :== {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9}
bwdII :== {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18}
bwdIII:== {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12}

notches :== {16, 4, 21}
reflectorB :== {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19}

mod26 :: Int -> Int
mod26 x = (x rem 26 + 26) rem 26

getFwd :: Int Int -> Int
getFwd 0 i = fwdI.[i]
getFwd 1 i = fwdII.[i]
getFwd 2 i = fwdIII.[i]

getBwd :: Int Int -> Int
getBwd 0 i = bwdI.[i]
getBwd 1 i = bwdII.[i]
getBwd 2 i = bwdIII.[i]

passFwd :: Int Int Int -> Int
passFwd rotor offset ch
    # inp = mod26 (ch + offset)
    # out = getFwd rotor inp
    = mod26 (out - offset)

passBwd :: Int Int Int -> Int
passBwd rotor offset ch
    # inp = mod26 (ch + offset)
    # out = getBwd rotor inp
    = mod26 (out - offset)

:: EnigmaState = { r0 :: Int, r1 :: Int, r2 :: Int
                 , o0 :: Int, o1 :: Int, o2 :: Int
                 , pb :: {Int}
                 }

makePlugboard :: [(Int,Int)] -> {Int}
makePlugboard pairs = foldl applyPair {i \\ i <- [0..25]} pairs
where
    applyPair pb (a,b) = {if (i==a) b (if (i==b) a pb.[i]) \\ i <- [0..25]}

stepAndEncrypt :: EnigmaState Int -> (Int, EnigmaState)
stepAndEncrypt st ch
    # mid = st.o1 == notches.[st.r1]
    # atn = st.o2 == notches.[st.r2]
    # no2 = mod26 (st.o2 + 1)
    # no1 = if (atn || mid) (mod26 (st.o1 + 1)) st.o1
    # no0 = if mid (mod26 (st.o0 + 1)) st.o0
    # c = st.pb.[ch]
    # c = passFwd st.r2 no2 c
    # c = passFwd st.r1 no1 c
    # c = passFwd st.r0 no0 c
    # c = reflectorB.[c]
    # c = passBwd st.r0 no0 c
    # c = passBwd st.r1 no1 c
    # c = passBwd st.r2 no2 c
    # c = st.pb.[c]
    = (c, {st & o0=no0, o1=no1, o2=no2})

encryptChars :: EnigmaState [Int] -> [Int]
encryptChars _ [] = []
encryptChars st [ch:rest]
    # (c, st`) = stepAndEncrypt st ch
    = [c : encryptChars st` rest]

encrypt :: (Int,Int,Int) (Int,Int,Int) [(Int,Int)] String -> String
encrypt (r0,r1,r2) (k0,k1,k2) pairs msg
    # pb = makePlugboard pairs
    # st = {r0=r0, r1=r1, r2=r2, o0=k0, o1=k1, o2=k2, pb=pb}
    # chars = [toInt c - toInt 'A' \\ c <-: msg]
    # result = encryptChars st chars
    = {toChar (i + toInt 'A') \\ i <- result}

runTest :: String String String *File -> *File
runTest label expected actual f
    # status = if (expected == actual) "PASS" "FAIL"
    # f = fwrites (status +++ " " +++ label +++ ": " +++ actual +++
                   " (expected " +++ expected +++ ")\n") f
    = f

Start :: *World -> *World
Start world
    # (console, world) = stdio world
    # console = fwrites "Enigma Cipher - Clean\n" console
    # console = runTest "Test 1" "BDZGO"
        (encrypt (0,1,2) (0,0,0) [] "AAAAA") console
    # console = runTest "Test 2" "ILBDAAMTAZ"
        (encrypt (0,1,2) (0,0,0) [] "HELLOWORLD") console
    # console = runTest "Test 3" "BZHGNOCRRTCM"
        (encrypt (0,1,2) (0,0,0) [] "ATTACKATDAWN") console
    # console = runTest "Test 4" "DLTBBQVPQV"
        (encrypt (0,1,2) (12,2,10) [] "HELLOWORLD") console
    # console = runTest "Test 5" "KZHDFQYHXT"
        (encrypt (2,0,1) (0,0,0) [] "HELLOWORLD") console
    # console = runTest "Test 6" "IKACBBMTBF"
        (encrypt (0,1,2) (0,0,0) [(0,1),(2,3),(4,5)] "HELLOWORLD") console
    # (_, world) = fclose console world
    = world
