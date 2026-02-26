-- Enigma Machine - PureScript Implementation
-- Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
-- PeopleTec Inc. - Guinness World Record Attempt 2026

module Main where

import Prelude
import Data.Array (index, updateAt, (..))
import Data.Char (toCharCode, fromCharCode)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (charAt, fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, new, read, write, modify)

fwdWirings :: Array String
fwdWirings = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"]

bwdWirings :: Array String
bwdWirings = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"]

notches :: Array Int
notches = [16, 4, 21]

reflector :: String
reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

mod26 :: Int -> Int
mod26 a = ((a `mod` 26) + 26) `mod` 26

c2i :: Char -> Int
c2i c = toCharCode c - toCharCode 'A'

i2c :: Int -> Char
i2c i = fromMaybe 'A' (fromCharCode (i + toCharCode 'A'))

charAtStr :: Int -> String -> Char
charAtStr i s = fromMaybe 'A' (charAt i s)

type Rotor = { fwd :: String, bwd :: String, notch :: Int, offset :: Ref Int }

makeRotor :: Int -> Char -> Effect Rotor
makeRotor num win = do
  off <- new (c2i win)
  pure { fwd: fromMaybe "" (index fwdWirings num)
       , bwd: fromMaybe "" (index bwdWirings num)
       , notch: fromMaybe 0 (index notches num)
       , offset: off }

fwdPass :: Rotor -> Int -> Effect Int
fwdPass r idx = do
  off <- read r.offset
  let contact = mod26 (idx + off)
  pure (mod26 (c2i (charAtStr contact r.fwd) - off))

bwdPass :: Rotor -> Int -> Effect Int
bwdPass r idx = do
  off <- read r.offset
  let contact = mod26 (idx + off)
  pure (mod26 (c2i (charAtStr contact r.bwd) - off))

stepRotor :: Rotor -> Effect Unit
stepRotor r = modify (\o -> (o + 1) `mod` 26) r.offset *> pure unit

atNotch :: Rotor -> Effect Boolean
atNotch r = do
  off <- read r.offset
  pure (off == r.notch)

type Enigma = { left :: Rotor, middle :: Rotor, right :: Rotor, plug :: Ref (Array Int) }

makeEnigma :: Array Int -> String -> Array String -> Effect Enigma
makeEnigma rotors key plugboard = do
  let basePlug = 0 .. 25
  plugRef <- new (foldl (\arr pair ->
    let a = c2i (charAtStr 0 pair)
        b = c2i (charAtStr 1 pair)
    in fromMaybe arr (updateAt a b arr >>= \arr' -> updateAt b a arr')
  ) basePlug plugboard)
  l <- makeRotor (fromMaybe 0 (index rotors 0)) (charAtStr 0 key)
  m <- makeRotor (fromMaybe 0 (index rotors 1)) (charAtStr 1 key)
  r <- makeRotor (fromMaybe 0 (index rotors 2)) (charAtStr 2 key)
  pure { left: l, middle: m, right: r, plug: plugRef }

stepRotors :: Enigma -> Effect Unit
stepRotors e = do
  mn <- atNotch e.middle
  rn <- atNotch e.right
  if mn then do stepRotor e.middle; stepRotor e.left
  else if rn then stepRotor e.middle
  else pure unit
  stepRotor e.right

pressKey :: Enigma -> Char -> Effect Char
pressKey e c = do
  stepRotors e
  p <- read e.plug
  let idx0 = c2i c
  let idx1 = fromMaybe idx0 (index p idx0)
  idx2 <- fwdPass e.right idx1
  idx3 <- fwdPass e.middle idx2
  idx4 <- fwdPass e.left idx3
  let idx5 = c2i (charAtStr idx4 reflector)
  idx6 <- bwdPass e.left idx5
  idx7 <- bwdPass e.middle idx6
  idx8 <- bwdPass e.right idx7
  let idx9 = fromMaybe idx8 (index p idx8)
  pure (i2c idx9)

encrypt :: Enigma -> String -> Effect String
encrypt e txt = do
  let chars = toCharArray txt # map \c ->
        if c >= 'a' && c <= 'z' then fromMaybe c (fromCharCode (toCharCode c - 32))
        else c
  let alphaChars = chars # filter \c -> c >= 'A' && c <= 'Z'
  results <- traverse (pressKey e) alphaChars
  pure (fromCharArray results)

main :: Effect Unit
main = do
  log "Enigma Machine - PureScript Implementation"
  log "==========================================="
  let tests =
        [ {rotors:[0,1,2], key:"AAA", plugs:[],              plain:"AAAAA",        expected:"BDZGO"}
        , {rotors:[0,1,2], key:"AAA", plugs:[],              plain:"HELLOWORLD",   expected:"ILBDAAMTAZ"}
        , {rotors:[0,1,2], key:"AAA", plugs:[],              plain:"ATTACKATDAWN", expected:"BZHGNOCRRTCM"}
        , {rotors:[0,1,2], key:"MCK", plugs:[],              plain:"HELLOWORLD",   expected:"DLTBBQVPQV"}
        , {rotors:[2,0,1], key:"AAA", plugs:[],              plain:"HELLOWORLD",   expected:"KZHDFQYHXT"}
        , {rotors:[0,1,2], key:"AAA", plugs:["AB","CD","EF"],plain:"HELLOWORLD",   expected:"IKACBBMTBF"}
        ]
  -- Run tests
  allPass <- new true
  foldl (\eff {t, i} -> eff >>= \_ -> do
    e <- makeEnigma t.rotors t.key t.plugs
    cipher <- encrypt e t.plain
    let ok = cipher == t.expected
    let status = if ok then "PASS" else "FAIL"
    log ("  Test " <> show i <> ": " <> t.plain <> " -> " <> cipher <> " [" <> status <> "]")
    unless ok do
      log ("          Expected " <> t.expected)
      write false allPass
  ) (pure unit) (tests # mapWithIndex \i t -> {t, i: i+1})
  ap <- read allPass
  log (if ap then "\n  ALL 6 TESTS PASSED" else "\n  SOME TESTS FAILED")
