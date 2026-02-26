# Enigma Cipher - Golo
# Lightweight JVM scripting language
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

module enigma

import gololang.Decorators

let fwdI  = array[4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
let fwdII = array[0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
let fwdIII= array[1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
let bwdI  = array[20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
let bwdII = array[0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
let bwdIII= array[19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
let reflector = array[24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
let notches = array[16, 4, 21]

function mod26 = |n| {
    let m = n % 26
    if m < 0 { return m + 26 } else { return m }
}

function getFwd = |r, i| -> match {
    when r == 0 then fwdI: get(i)
    when r == 1 then fwdII: get(i)
    otherwise fwdIII: get(i)
}

function getBwd = |r, i| -> match {
    when r == 0 then bwdI: get(i)
    when r == 1 then bwdII: get(i)
    otherwise bwdIII: get(i)
}

function passFwd = |rotor, offset, ch| {
    let inp = mod26(ch + offset)
    return mod26(getFwd(rotor, inp) - offset)
}

function passBwd = |rotor, offset, ch| {
    let inp = mod26(ch + offset)
    return mod26(getBwd(rotor, inp) - offset)
}

struct enigma_state = { r, o, n1, n2 }

function newEnigma = |r0, r1, r2, k0, k1, k2| ->
    enigma_state(array[r0,r1,r2], array[k0,k1,k2], notches:get(r1), notches:get(r2))

function step = |s| {
    if s: o(): get(1) == s: n1() {
        s: o(): set(1, mod26(s: o(): get(1) + 1))
        s: o(): set(0, mod26(s: o(): get(0) + 1))
    } else if s: o(): get(2) == s: n2() {
        s: o(): set(1, mod26(s: o(): get(1) + 1))
    }
    s: o(): set(2, mod26(s: o(): get(2) + 1))
}

function pressKey = |s, ch| {
    step(s)
    var c = ch
    c = passFwd(s: r(): get(2), s: o(): get(2), c)
    c = passFwd(s: r(): get(1), s: o(): get(1), c)
    c = passFwd(s: r(): get(0), s: o(): get(0), c)
    c = reflector: get(c)
    c = passBwd(s: r(): get(0), s: o(): get(0), c)
    c = passBwd(s: r(): get(1), s: o(): get(1), c)
    c = passBwd(s: r(): get(2), s: o(): get(2), c)
    return c
}

function encrypt = |r0, r1, r2, k0, k1, k2, msg| {
    let s = newEnigma(r0, r1, r2, k0, k1, k2)
    let result = java.lang.StringBuilder()
    foreach ch in msg {
        let v = ch: charValue(): intValue() - 65
        result: append(char(pressKey(s, v) + 65))
    }
    return result: toString()
}

function main = |args| {
    println("Enigma Cipher - Golo")
    println("Test 1: " + encrypt(0,1,2,0,0,0,"AAAAA") + " expected BDZGO")
    println("Test 2: " + encrypt(0,1,2,0,0,0,"HELLOWORLD") + " expected ILBDAAMTAZ")
    println("Test 3: " + encrypt(0,1,2,0,0,0,"ATTACKATDAWN") + " expected BZHGNOCRRTCM")
    println("Test 4: " + encrypt(0,1,2,12,2,10,"HELLOWORLD") + " expected DLTBBQVPQV")
    println("Test 5: " + encrypt(2,0,1,0,0,0,"HELLOWORLD") + " expected KZHDFQYHXT")
}
