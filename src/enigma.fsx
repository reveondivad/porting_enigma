// Enigma Machine - F# Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

let fwdW = [| "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
              "AJDKSIRUXBLHWTMCQGZNPYFVOE"
              "BDFHJLCPRTXVZNYEIWGAKMUSQO" |]

let bwdW = [| "UWYGADFPVZBECKMTHXSLRINQOJ"
              "AJPCZWRLFBDKOTYUQGENHXMIVS"
              "TAGBPCSDQEUFVNZHYIXJWLRKOM" |]

let notchPos = [| 16; 4; 21 |]
let refl = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

let mod26 a = ((a % 26) + 26) % 26
let c2i (c: char) = int c - int 'A'
let i2c i = char (i + int 'A')

type Rotor = { Fwd: string; Bwd: string; Notch: int; mutable Offset: int }

let makeRotor num win =
    { Fwd = fwdW.[num]; Bwd = bwdW.[num]; Notch = notchPos.[num]; Offset = c2i win }

let fwdPass (r: Rotor) idx =
    let contact = mod26 (idx + r.Offset)
    mod26 (c2i r.Fwd.[contact] - r.Offset)

let bwdPass (r: Rotor) idx =
    let contact = mod26 (idx + r.Offset)
    mod26 (c2i r.Bwd.[contact] - r.Offset)

let step (r: Rotor) = r.Offset <- (r.Offset + 1) % 26

type Enigma = { Left: Rotor; Middle: Rotor; Right: Rotor; Plug: int array }

let makeEnigma (r1, r2, r3) (key: string) plugboard =
    let plug = Array.init 26 id
    plugboard |> List.iter (fun (pair: string) ->
        let a, b = c2i pair.[0], c2i pair.[1]
        plug.[a] <- b; plug.[b] <- a)
    { Left = makeRotor r1 key.[0]; Middle = makeRotor r2 key.[1]
      Right = makeRotor r3 key.[2]; Plug = plug }

let stepRotors (e: Enigma) =
    if e.Middle.Offset = e.Middle.Notch then
        step e.Middle; step e.Left
    elif e.Right.Offset = e.Right.Notch then
        step e.Middle
    step e.Right

let pressKey (e: Enigma) c =
    stepRotors e
    let mutable idx = c2i c
    idx <- e.Plug.[idx]
    idx <- fwdPass e.Right idx
    idx <- fwdPass e.Middle idx
    idx <- fwdPass e.Left idx
    idx <- c2i refl.[idx]
    idx <- bwdPass e.Left idx
    idx <- bwdPass e.Middle idx
    idx <- bwdPass e.Right idx
    idx <- e.Plug.[idx]
    i2c idx

let encrypt (e: Enigma) (text: string) =
    text.ToUpper()
    |> Seq.filter System.Char.IsLetter
    |> Seq.map (pressKey e)
    |> System.String.Concat

[<EntryPoint>]
let main _ =
    printfn "Enigma Machine - F# Implementation"
    printfn "===================================="
    let tests = [
        (0,1,2), "AAA", [],              "AAAAA",        "BDZGO"
        (0,1,2), "AAA", [],              "HELLOWORLD",   "ILBDAAMTAZ"
        (0,1,2), "AAA", [],              "ATTACKATDAWN", "BZHGNOCRRTCM"
        (0,1,2), "MCK", [],              "HELLOWORLD",   "DLTBBQVPQV"
        (2,0,1), "AAA", [],              "HELLOWORLD",   "KZHDFQYHXT"
        (0,1,2), "AAA", ["AB";"CD";"EF"],"HELLOWORLD",   "IKACBBMTBF"
    ]
    let mutable allPass = true
    tests |> List.iteri (fun i (rotors, key, plugs, plain, expected) ->
        let e = makeEnigma rotors key plugs
        let cipher = encrypt e plain
        let ok = cipher = expected
        let status = if ok then "PASS" else "FAIL"
        printfn "  Test %d: %-20s -> %-15s [%s]" (i+1) plain cipher status
        if not ok then
            printfn "          Expected %s, got %s" expected cipher
            allPass <- false)
    printfn "\n  %s" (if allPass then "ALL 6 TESTS PASSED" else "SOME TESTS FAILED")
    0
