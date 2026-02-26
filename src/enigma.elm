-- Enigma Machine - Elm Implementation
-- Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
-- PeopleTec Inc. - Guinness World Record Attempt 2026
-- Note: Elm is purely functional; outputs via Html. This is a self-contained module.

module Enigma exposing (main)

import Html exposing (Html, div, text, pre)
import Array exposing (Array)

fwd : Array String
fwd = Array.fromList ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"]

bwd : Array String
bwd = Array.fromList ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"]

notches : Array Int
notches = Array.fromList [16, 4, 21]

reflector : String
reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

mod26 : Int -> Int
mod26 a = modBy 26 (modBy 26 a + 26)

c2i : Char -> Int
c2i c = Char.toCode c - Char.toCode 'A'

i2c : Int -> Char
i2c i = Char.fromCode (i + Char.toCode 'A')

charAt : Int -> String -> Char
charAt i s = String.toList s |> List.drop i |> List.head |> Maybe.withDefault 'A'

type alias Rotor = { fwd : String, bwd : String, notch : Int, offset : Int }

makeRotor : Int -> Char -> Rotor
makeRotor num win =
    { fwd = Array.get num fwd |> Maybe.withDefault ""
    , bwd = Array.get num bwd |> Maybe.withDefault ""
    , notch = Array.get num notches |> Maybe.withDefault 0
    , offset = c2i win }

fwdPass : Rotor -> Int -> Int
fwdPass r idx =
    let contact = mod26 (idx + r.offset)
        out = c2i (charAt contact r.fwd)
    in mod26 (out - r.offset)

bwdPass : Rotor -> Int -> Int
bwdPass r idx =
    let contact = mod26 (idx + r.offset)
        out = c2i (charAt contact r.bwd)
    in mod26 (out - r.offset)

stepRotor : Rotor -> Rotor
stepRotor r = { r | offset = modBy 26 (r.offset + 1) }

atNotch : Rotor -> Bool
atNotch r = r.offset == r.notch

type alias Enigma = { left : Rotor, middle : Rotor, right : Rotor, plug : Array Int }

makeEnigma : List Int -> String -> List String -> Enigma
makeEnigma rotors key plugboard =
    let basePlug = Array.initialize 26 identity
        plug = List.foldl (\pair arr ->
            let a = c2i (charAt 0 pair)
                b = c2i (charAt 1 pair)
            in arr |> Array.set a b |> Array.set b a
        ) basePlug plugboard
        k = String.toList key
        k0 = List.head k |> Maybe.withDefault 'A'
        k1 = List.drop 1 k |> List.head |> Maybe.withDefault 'A'
        k2 = List.drop 2 k |> List.head |> Maybe.withDefault 'A'
        r = rotors
        r0 = List.head r |> Maybe.withDefault 0
        r1 = List.drop 1 r |> List.head |> Maybe.withDefault 0
        r2 = List.drop 2 r |> List.head |> Maybe.withDefault 0
    in { left = makeRotor r0 k0, middle = makeRotor r1 k1, right = makeRotor r2 k2, plug = plug }

stepRotors : Enigma -> Enigma
stepRotors e =
    let (l, m) =
            if atNotch e.middle then (stepRotor e.left, stepRotor e.middle)
            else if atNotch e.right then (e.left, stepRotor e.middle)
            else (e.left, e.middle)
        r = stepRotor e.right
    in { e | left = l, middle = m, right = r }

pressKey : Enigma -> Char -> (Enigma, Char)
pressKey e c =
    let e1 = stepRotors e
        idx = c2i c
        idx1 = Array.get idx e1.plug |> Maybe.withDefault idx
        idx2 = fwdPass e1.right idx1
        idx3 = fwdPass e1.middle idx2
        idx4 = fwdPass e1.left idx3
        idx5 = c2i (charAt idx4 reflector)
        idx6 = bwdPass e1.left idx5
        idx7 = bwdPass e1.middle idx6
        idx8 = bwdPass e1.right idx7
        idx9 = Array.get idx8 e1.plug |> Maybe.withDefault idx8
    in (e1, i2c idx9)

encrypt : Enigma -> String -> String
encrypt e txt =
    let chars = String.toList (String.toUpper txt) |> List.filter Char.isAlpha
        (_, result) = List.foldl (\c (eng, acc) ->
            let (eng2, out) = pressKey eng c
            in (eng2, acc ++ [out])
        ) (e, []) chars
    in String.fromList result

type alias Test = { rotors : List Int, key : String, plugs : List String, plain : String, expected : String }

tests : List Test
tests =
    [ { rotors=[0,1,2], key="AAA", plugs=[],               plain="AAAAA",        expected="BDZGO" }
    , { rotors=[0,1,2], key="AAA", plugs=[],               plain="HELLOWORLD",   expected="ILBDAAMTAZ" }
    , { rotors=[0,1,2], key="AAA", plugs=[],               plain="ATTACKATDAWN", expected="BZHGNOCRRTCM" }
    , { rotors=[0,1,2], key="MCK", plugs=[],               plain="HELLOWORLD",   expected="DLTBBQVPQV" }
    , { rotors=[2,0,1], key="AAA", plugs=[],               plain="HELLOWORLD",   expected="KZHDFQYHXT" }
    , { rotors=[0,1,2], key="AAA", plugs=["AB","CD","EF"], plain="HELLOWORLD",   expected="IKACBBMTBF" }
    ]

runTests : List String
runTests =
    List.indexedMap (\i t ->
        let e = makeEnigma t.rotors t.key t.plugs
            cipher = encrypt e t.plain
            ok = cipher == t.expected
            status = if ok then "PASS" else "FAIL"
        in "  Test " ++ String.fromInt (i+1) ++ ": " ++ t.plain ++ " -> " ++ cipher ++ " [" ++ status ++ "]"
    ) tests

main : Html msg
main =
    let results = runTests
        allPass = List.all (\s -> String.contains "PASS" s) results
        footer = if allPass then "\n  ALL 6 TESTS PASSED" else "\n  SOME TESTS FAILED"
    in pre [] [ text ("Enigma Machine - Elm Implementation\n=======================================\n"
                      ++ String.join "\n" results ++ footer ++ "\n") ]
