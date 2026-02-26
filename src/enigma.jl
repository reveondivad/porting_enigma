# Enigma Machine - Julia Implementation
# Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
# PeopleTec Inc. - Guinness World Record Attempt 2026

const FWD_W = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ",
               "AJDKSIRUXBLHWTMCQGZNPYFVOE",
               "BDFHJLCPRTXVZNYEIWGAKMUSQO"]
const BWD_W = ["UWYGADFPVZBECKMTHXSLRINQOJ",
               "AJPCZWRLFBDKOTYUQGENHXMIVS",
               "TAGBPCSDQEUFVNZHYIXJWLRKOM"]
const NOTCH_POS = [17, 5, 22]  # 1-based: Q=17, E=5, V=22
const REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

mod26(a) = mod(a + 260, 26)  # always positive

mutable struct Rotor
    fwd::String
    bwd::String
    notch::Int
    offset::Int
end

function make_rotor(num::Int, win::Char)
    Rotor(FWD_W[num], BWD_W[num], NOTCH_POS[num], Int(win) - Int('A') + 1)
end

function fwd_pass(r::Rotor, idx::Int)
    contact = mod26(idx - 1 + r.offset - 1) + 1  # 1-based
    out = Int(r.fwd[contact]) - Int('A') + 1
    mod26(out - 1 - (r.offset - 1)) + 1
end

function bwd_pass(r::Rotor, idx::Int)
    contact = mod26(idx - 1 + r.offset - 1) + 1
    out = Int(r.bwd[contact]) - Int('A') + 1
    mod26(out - 1 - (r.offset - 1)) + 1
end

step!(r::Rotor) = (r.offset = mod(r.offset, 26) + 1)
at_notch(r::Rotor) = r.offset == r.notch

mutable struct Enigma
    left::Rotor
    middle::Rotor
    right::Rotor
    plug::Vector{Int}
end

function make_enigma(rotors, key::String, plugboard::Vector{String}=String[])
    plug = collect(1:26)
    for pair in plugboard
        a = Int(pair[1]) - Int('A') + 1
        b = Int(pair[2]) - Int('A') + 1
        plug[a] = b; plug[b] = a
    end
    Enigma(make_rotor(rotors[1], key[1]),
           make_rotor(rotors[2], key[2]),
           make_rotor(rotors[3], key[3]), plug)
end

function step_rotors!(e::Enigma)
    if at_notch(e.middle)
        step!(e.middle); step!(e.left)
    elseif at_notch(e.right)
        step!(e.middle)
    end
    step!(e.right)
end

function press_key(e::Enigma, c::Char)
    step_rotors!(e)
    idx = Int(c) - Int('A') + 1
    idx = e.plug[idx]
    idx = fwd_pass(e.right, idx)
    idx = fwd_pass(e.middle, idx)
    idx = fwd_pass(e.left, idx)
    idx = Int(REFL[idx]) - Int('A') + 1
    idx = bwd_pass(e.left, idx)
    idx = bwd_pass(e.middle, idx)
    idx = bwd_pass(e.right, idx)
    idx = e.plug[idx]
    Char(idx - 1 + Int('A'))
end

function encrypt(e::Enigma, text::String)
    join([press_key(e, c) for c in uppercase(text) if 'A' <= c <= 'Z'])
end

# Test harness
println("Enigma Machine - Julia Implementation")
println("======================================")

tests = [
    ([1,2,3], "AAA", String[],         "AAAAA",        "BDZGO"),
    ([1,2,3], "AAA", String[],         "HELLOWORLD",   "ILBDAAMTAZ"),
    ([1,2,3], "AAA", String[],         "ATTACKATDAWN", "BZHGNOCRRTCM"),
    ([1,2,3], "MCK", String[],         "HELLOWORLD",   "DLTBBQVPQV"),
    ([3,1,2], "AAA", String[],         "HELLOWORLD",   "KZHDFQYHXT"),
    ([1,2,3], "AAA", ["AB","CD","EF"], "HELLOWORLD",   "IKACBBMTBF"),
]

all_pass = true
for (i, (rotors, key, plugs, plain, expected)) in enumerate(tests)
    e = make_enigma(rotors, key, plugs)
    cipher = encrypt(e, plain)
    ok = cipher == expected
    status = ok ? "PASS" : "FAIL"
    @printf("  Test %d: %-20s -> %-15s [%s]\n", i, plain, cipher, status)
    if !ok
        @printf("          Expected %s, got %s\n", expected, cipher)
        global all_pass = false
    end
end
println(all_pass ? "\n  ALL 6 TESTS PASSED" : "\n  SOME TESTS FAILED")
