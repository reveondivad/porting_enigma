-- Enigma Cipher - Agda
-- Dependently typed functional programming / proof assistant

module Enigma where

open import Data.Nat using (ℕ; zero; suc; _+_; _∸_; _%_; _≡ᵇ_)
open import Data.List using (List; []; _∷_; map; foldl)
open import Data.String using (String; toList; fromList)
open import Data.Char using (Char; toℕ; fromℕ)
open import Data.Bool using (Bool; true; false; if_then_else_; _∨_)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import IO

fwdI : List ℕ
fwdI = 4 ∷ 10 ∷ 12 ∷ 5 ∷ 11 ∷ 6 ∷ 3 ∷ 16 ∷ 21 ∷ 25 ∷ 13 ∷ 19 ∷ 14 ∷ 22 ∷ 24 ∷ 7 ∷ 23 ∷ 20 ∷ 18 ∷ 15 ∷ 0 ∷ 8 ∷ 1 ∷ 17 ∷ 2 ∷ 9 ∷ []

fwdII : List ℕ
fwdII = 0 ∷ 9 ∷ 3 ∷ 10 ∷ 18 ∷ 8 ∷ 17 ∷ 20 ∷ 23 ∷ 1 ∷ 11 ∷ 7 ∷ 22 ∷ 19 ∷ 12 ∷ 2 ∷ 16 ∷ 6 ∷ 25 ∷ 13 ∷ 15 ∷ 24 ∷ 5 ∷ 21 ∷ 14 ∷ 4 ∷ []

fwdIII : List ℕ
fwdIII = 1 ∷ 3 ∷ 5 ∷ 7 ∷ 9 ∷ 11 ∷ 2 ∷ 15 ∷ 17 ∷ 19 ∷ 23 ∷ 21 ∷ 25 ∷ 13 ∷ 24 ∷ 4 ∷ 8 ∷ 22 ∷ 6 ∷ 0 ∷ 10 ∷ 12 ∷ 20 ∷ 18 ∷ 16 ∷ 14 ∷ []

bwdI : List ℕ
bwdI = 20 ∷ 22 ∷ 24 ∷ 6 ∷ 0 ∷ 3 ∷ 5 ∷ 15 ∷ 21 ∷ 25 ∷ 1 ∷ 4 ∷ 2 ∷ 10 ∷ 12 ∷ 19 ∷ 7 ∷ 23 ∷ 18 ∷ 11 ∷ 17 ∷ 8 ∷ 13 ∷ 16 ∷ 14 ∷ 9 ∷ []

bwdII : List ℕ
bwdII = 0 ∷ 9 ∷ 15 ∷ 2 ∷ 25 ∷ 22 ∷ 17 ∷ 11 ∷ 5 ∷ 1 ∷ 3 ∷ 10 ∷ 14 ∷ 19 ∷ 24 ∷ 20 ∷ 16 ∷ 6 ∷ 4 ∷ 13 ∷ 7 ∷ 23 ∷ 12 ∷ 8 ∷ 21 ∷ 18 ∷ []

bwdIII : List ℕ
bwdIII = 19 ∷ 0 ∷ 6 ∷ 1 ∷ 15 ∷ 2 ∷ 18 ∷ 3 ∷ 16 ∷ 4 ∷ 20 ∷ 9 ∷ 21 ∷ 13 ∷ 25 ∷ 7 ∷ 24 ∷ 8 ∷ 23 ∷ 5 ∷ 22 ∷ 11 ∷ 17 ∷ 12 ∷ 14 ∷ 10 ∷ []

notches : List ℕ
notches = 16 ∷ 4 ∷ 21 ∷ []

reflectorB : List ℕ
reflectorB = 24 ∷ 17 ∷ 20 ∷ 7 ∷ 16 ∷ 18 ∷ 11 ∷ 3 ∷ 15 ∷ 23 ∷ 13 ∷ 6 ∷ 14 ∷ 10 ∷ 12 ∷ 8 ∷ 4 ∷ 1 ∷ 5 ∷ 25 ∷ 2 ∷ 22 ∷ 21 ∷ 9 ∷ 0 ∷ 19 ∷ []

mod26 : ℕ → ℕ
mod26 x = x % 26

nth : List ℕ → ℕ → ℕ
nth [] _ = 0
nth (x ∷ _) zero = x
nth (_ ∷ xs) (suc n) = nth xs n

getFwd : ℕ → List ℕ
getFwd zero = fwdI
getFwd (suc zero) = fwdII
getFwd _ = fwdIII

getBwd : ℕ → List ℕ
getBwd zero = bwdI
getBwd (suc zero) = bwdII
getBwd _ = bwdIII

getNotch : ℕ → ℕ
getNotch r = nth notches r

passFwd : ℕ → ℕ → ℕ → ℕ
passFwd rotor offset ch =
  let inp = mod26 (ch + offset)
      out = nth (getFwd rotor) inp
  in mod26 ((out + 26) ∸ offset)

passBwd : ℕ → ℕ → ℕ → ℕ
passBwd rotor offset ch =
  let inp = mod26 (ch + offset)
      out = nth (getBwd rotor) inp
  in mod26 ((out + 26) ∸ offset)

record State : Set where
  constructor mkState
  field
    r0 r1 r2 : ℕ
    o0 o1 o2 : ℕ
    pb : List (ℕ × ℕ)

applyPB : List (ℕ × ℕ) → ℕ → ℕ
applyPB [] ch = ch
applyPB ((a , b) ∷ rest) ch =
  if a ≡ᵇ ch then b
  else if b ≡ᵇ ch then a
  else applyPB rest ch

stepEncrypt : State → ℕ → ℕ × State
stepEncrypt st ch =
  let mid = State.o1 st ≡ᵇ getNotch (State.r1 st)
      atn = State.o2 st ≡ᵇ getNotch (State.r2 st)
      no2 = mod26 (State.o2 st + 1)
      no1 = if atn ∨ mid then mod26 (State.o1 st + 1) else State.o1 st
      no0 = if mid then mod26 (State.o0 st + 1) else State.o0 st
      c = applyPB (State.pb st) ch
      c₁ = passFwd (State.r2 st) no2 c
      c₂ = passFwd (State.r1 st) no1 c₁
      c₃ = passFwd (State.r0 st) no0 c₂
      c₄ = nth reflectorB c₃
      c₅ = passBwd (State.r0 st) no0 c₄
      c₆ = passBwd (State.r1 st) no1 c₅
      c₇ = passBwd (State.r2 st) no2 c₆
      c₈ = applyPB (State.pb st) c₇
  in c₈ , mkState (State.r0 st) (State.r1 st) (State.r2 st) no0 no1 no2 (State.pb st)

encryptAll : State → List ℕ → List ℕ
encryptAll _ [] = []
encryptAll st (ch ∷ rest) =
  let (c , st') = stepEncrypt st ch
  in c ∷ encryptAll st' rest

-- Test: encrypt [0,0,0,0,0] with rotors (0,1,2) key (0,0,0) no plugboard
-- Expected: [1,3,25,6,14] (BDZGO)
test1 : List ℕ
test1 = encryptAll (mkState 0 1 2 0 0 0 []) (0 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ [])
