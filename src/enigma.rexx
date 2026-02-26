/* Enigma Machine - REXX Implementation */
/* Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping) */
/* PeopleTec Inc. - Guinness World Record Attempt 2026 */

fwd.1 = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
fwd.2 = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
fwd.3 = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
bwd.1 = "UWYGADFPVZBECKMTHXSLRINQOJ"
bwd.2 = "AJPCZWRLFBDKOTYUQGENHXMIVS"
bwd.3 = "TAGBPCSDQEUFVNZHYIXJWLRKOM"
ref = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
notch.1 = 16; notch.2 = 4; notch.3 = 21

say "Enigma Machine - REXX Implementation"
say "====================================="

/* Test 1 */
call initEnigma 1, 2, 3, "A", "A", "A", ""
result = encrypt("AAAAA")
call showTest 1, "AAAAA", result, "BDZGO"

/* Test 2 */
call initEnigma 1, 2, 3, "A", "A", "A", ""
result = encrypt("HELLOWORLD")
call showTest 2, "HELLOWORLD", result, "ILBDAAMTAZ"

/* Test 3 */
call initEnigma 1, 2, 3, "A", "A", "A", ""
result = encrypt("ATTACKATDAWN")
call showTest 3, "ATTACKATDAWN", result, "BZHGNOCRRTCM"

/* Test 4 */
call initEnigma 1, 2, 3, "M", "C", "K", ""
result = encrypt("HELLOWORLD")
call showTest 4, "HELLOWORLD", result, "DLTBBQVPQV"

/* Test 5 */
call initEnigma 3, 1, 2, "A", "A", "A", ""
result = encrypt("HELLOWORLD")
call showTest 5, "HELLOWORLD", result, "KZHDFQYHXT"

/* Test 6 */
call initEnigma 1, 2, 3, "A", "A", "A", "AB-CD-EF"
result = encrypt("HELLOWORLD")
call showTest 6, "HELLOWORLD", result, "IKACBBMTBF"

exit

showTest: procedure
    parse arg num, input, result, expected
    if result = expected then tag = "[PASS]"
    else tag = "[FAIL] expected" expected
    say "Test" num":" input "->" result tag
    return

initEnigma: procedure expose rotor. offset. rnotch. plug.
    parse arg r1, r2, r3, k1, k2, k3, plugPairs
    rotor.1 = r1; rotor.2 = r2; rotor.3 = r3
    offset.1 = c2i(k1); offset.2 = c2i(k2); offset.3 = c2i(k3)
    rnotch.1 = getNotch(r1); rnotch.2 = getNotch(r2); rnotch.3 = getNotch(r3)
    do i = 0 to 25
        plug.i = i
    end
    if plugPairs \= "" then do
        do while plugPairs \= ""
            parse var plugPairs pair "-" plugPairs
            a = c2i(substr(pair, 1, 1))
            b = c2i(substr(pair, 2, 1))
            plug.a = b
            plug.b = a
        end
    end
    return

getNotch: procedure expose notch.
    parse arg r
    return notch.r

c2i: procedure
    parse arg c
    return c2d(c) - c2d("A")

i2c: procedure
    parse arg n
    return d2c(n + c2d("A"))

mod26: procedure
    parse arg n
    return ((n // 26) + 26) // 26

stepRotors: procedure expose offset. rnotch.
    if offset.2 = rnotch.2 then do
        offset.2 = mod26(offset.2 + 1)
        offset.1 = mod26(offset.1 + 1)
    end
    else if offset.3 = rnotch.3 then do
        offset.2 = mod26(offset.2 + 1)
    end
    offset.3 = mod26(offset.3 + 1)
    return

fwdPass: procedure expose rotor. offset. fwd.
    parse arg slot, idx
    r = rotor.slot
    contact = mod26(idx + offset.slot)
    out = c2i(substr(fwd.r, contact + 1, 1))
    return mod26(out - offset.slot)

bwdPass: procedure expose rotor. offset. bwd.
    parse arg slot, idx
    r = rotor.slot
    contact = mod26(idx + offset.slot)
    out = c2i(substr(bwd.r, contact + 1, 1))
    return mod26(out - offset.slot)

pressKey: procedure expose rotor. offset. rnotch. plug. fwd. bwd. ref
    parse arg ch
    call stepRotors
    idx = plug.c2i(ch) + 0
    pv = plug.idx
    idx = fwdPass(3, pv)
    idx = fwdPass(2, idx)
    idx = fwdPass(1, idx)
    idx = c2i(substr(ref, idx + 1, 1))
    idx = bwdPass(1, idx)
    idx = bwdPass(2, idx)
    idx = bwdPass(3, idx)
    pidx = plug.idx
    return i2c(pidx)

encrypt: procedure expose rotor. offset. rnotch. plug. fwd. bwd. ref
    parse arg text
    text = translate(text)  /* uppercase */
    result = ""
    do i = 1 to length(text)
        ch = substr(text, i, 1)
        if datatype(ch, "U") then
            result = result || pressKey(ch)
    end
    return result