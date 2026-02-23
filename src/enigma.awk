#!/usr/bin/awk -f
# Enigma Machine — Rosetta Code Reference (AWK)
# Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
# PeopleTec Inc. — Guinness World Record Attempt 2026

BEGIN {
    split("EKMFLGDQVZNTOWYHXUSPAIBRCJ,AJDKSIRUXBLHWTMCQGZNPYFVOE,BDFHJLCPRTXVZNYEIWGAKMUSQO", FWD, ",")
    split("UWYGADFPVZBECKMTHXSLRINQOJ,AJPCZWRLFBDKOTYUQGENHXMIVS,TAGBPCSDQEUFVNZHYIXJWLRKOM", BWD, ",")
    split("Q,E,V", NOTCH, ",")
    REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
    ALPHA = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    all_ok = 1
    test(1, "0,1,2", "AAA", "", "AAAAA", "BDZGO")
    test(2, "0,1,2", "AAA", "", "HELLOWORLD", "ILBDAAMTAZ")
    test(3, "0,1,2", "AAA", "", "ATTACKATDAWN", "BZHGNOCRRTCM")
    test(4, "0,1,2", "MCK", "", "HELLOWORLD", "DLTBBQVPQV")
    test(5, "2,0,1", "AAA", "", "HELLOWORLD", "KZHDFQYHXT")
    test(6, "0,1,2", "AAA", "AB,CD,EF", "HELLOWORLD", "IKACBBMTBF")
    printf "%s\n", (all_ok ? "\nALL 6 TESTS PASSED" : "\nSOME TESTS FAILED")
}

function ci(c) { return index(ALPHA, c) - 1 }
function cc(i) { return substr(ALPHA, i+1, 1) }

function init_enigma(rotors_s, key, plugs_s,   n,i,a,b,rr,pp) {
    split(rotors_s, rr, ",")
    for (i=1;i<=3;i++) { id[i]=rr[i]+1; off[i]=ci(substr(key,i,1)) }
    for (i=0;i<26;i++) plug[i]=i
    if (plugs_s != "") {
        n=split(plugs_s, pp, ",")
        for (i=1;i<=n;i++) {
            a=ci(substr(pp[i],1,1)); b=ci(substr(pp[i],2,1))
            plug[a]=b; plug[b]=a
        }
    }
}

function fwd_pass(r, i,  c,out) {
    c=(i+off[r])%26
    out=ci(substr(FWD[id[r]],c+1,1))
    return (out-off[r]+26)%26
}

function bwd_pass(r, i,  c,out) {
    c=(i+off[r])%26
    out=ci(substr(BWD[id[r]],c+1,1))
    return (out-off[r]+26)%26
}

function do_step() {
    if (off[2] == ci(NOTCH[id[2]])) {
        off[2]=(off[2]+1)%26; off[1]=(off[1]+1)%26
    } else if (off[3] == ci(NOTCH[id[3]])) {
        off[2]=(off[2]+1)%26
    }
    off[3]=(off[3]+1)%26
}

function press_key(ch,  i) {
    do_step()
    i = ci(ch)
    i = plug[i]
    i = fwd_pass(3,i); i = fwd_pass(2,i); i = fwd_pass(1,i)
    i = ci(substr(REFLECTOR,i+1,1))
    i = bwd_pass(1,i); i = bwd_pass(2,i); i = bwd_pass(3,i)
    i = plug[i]
    return cc(i)
}

function encrypt(text,  i,c,out) {
    out = ""
    text = toupper(text)
    gsub(/[^A-Z]/,"",text)
    for (i=1; i<=length(text); i++) {
        c = substr(text,i,1)
        out = out press_key(c)
    }
    return out
}

function test(num, rotors, key, plugs, plain, expected,  ct,ok) {
    init_enigma(rotors, key, plugs)
    ct = encrypt(plain)
    ok = (ct == expected)
    printf "Test %d: %-20s -> %-15s [%s]\n", num, plain, ct, (ok ? "PASS" : "FAIL")
    if (!ok) all_ok = 0
}
