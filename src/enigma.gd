# Enigma Cipher - GDScript
# Godot game engine scripting language

extends Node

const FWD := [
    [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
    [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
    [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
]

const BWD := [
    [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
    [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
    [19,0,6,1,15,2,18,3,16,4,20,9,21,13,25,7,24,8,23,5,22,11,17,12,14,10]
]

const NOTCH := [16, 4, 21]
const REFLECTOR := [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]

func mod26(x: int) -> int:
    var m := x % 26
    if m < 0:
        m += 26
    return m

func pass_fwd(rotor: int, offset: int, ch: int) -> int:
    var inp := mod26(ch + offset)
    var out := FWD[rotor][inp]
    return mod26(out - offset)

func pass_bwd(rotor: int, offset: int, ch: int) -> int:
    var inp := mod26(ch + offset)
    var out := BWD[rotor][inp]
    return mod26(out - offset)

func make_plugboard(pairs: Array) -> Array:
    var pb := []
    for i in range(26):
        pb.append(i)
    for p in pairs:
        pb[p[0]] = p[1]
        pb[p[1]] = p[0]
    return pb

func encrypt(r0: int, r1: int, r2: int,
             k0: int, k1: int, k2: int,
             pairs: Array, msg: String) -> String:
    var pb := make_plugboard(pairs)
    var o0 := k0
    var o1 := k1
    var o2 := k2
    var result := ""
    for i in range(msg.length()):
        var ch := msg.unicode_at(i) - 65
        var mid := o1 == NOTCH[r1]
        var atn := o2 == NOTCH[r2]
        o2 = mod26(o2 + 1)
        if atn or mid:
            o1 = mod26(o1 + 1)
        if mid:
            o0 = mod26(o0 + 1)
        var c := pb[ch]
        c = pass_fwd(r2, o2, c)
        c = pass_fwd(r1, o1, c)
        c = pass_fwd(r0, o0, c)
        c = REFLECTOR[c]
        c = pass_bwd(r0, o0, c)
        c = pass_bwd(r1, o1, c)
        c = pass_bwd(r2, o2, c)
        c = pb[c]
        result += char(c + 65)
    return result

func run_test(label: String, expected: String, actual: String) -> void:
    var status := "PASS" if expected == actual else "FAIL"
    print("%s %s: %s (expected %s)" % [status, label, actual, expected])

func _ready() -> void:
    print("Enigma Cipher - GDScript")
    run_test("Test 1", "BDZGO",
        encrypt(0,1,2, 0,0,0, [], "AAAAA"))
    run_test("Test 2", "ILBDAAMTAZ",
        encrypt(0,1,2, 0,0,0, [], "HELLOWORLD"))
    run_test("Test 3", "BZHGNOCRRTCM",
        encrypt(0,1,2, 0,0,0, [], "ATTACKATDAWN"))
    run_test("Test 4", "DLTBBQVPQV",
        encrypt(0,1,2, 12,2,10, [], "HELLOWORLD"))
    run_test("Test 5", "KZHDFQYHXT",
        encrypt(2,0,1, 0,0,0, [], "HELLOWORLD"))
    run_test("Test 6", "IKACBBMTBF",
        encrypt(0,1,2, 0,0,0, [[0,1],[2,3],[4,5]], "HELLOWORLD"))
