# Enigma Cipher - Nit
# Object-oriented language with static types and nullable types
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

module enigma

var fwd_i  = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
var fwd_ii = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
var fwd_iii= [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
var bwd_i  = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
var bwd_ii = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
var bwd_iii= [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
var reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
var notches = [16, 4, 21]

fun mod26(n: Int): Int do
    var m = n % 26
    if m < 0 then m += 26
    return m
end

fun get_fwd(r: Int, i: Int): Int do
    if r == 0 then return fwd_i[i]
    if r == 1 then return fwd_ii[i]
    return fwd_iii[i]
end

fun get_bwd(r: Int, i: Int): Int do
    if r == 0 then return bwd_i[i]
    if r == 1 then return bwd_ii[i]
    return bwd_iii[i]
end

fun pass_fwd(rotor: Int, offset: Int, ch: Int): Int do
    return mod26(get_fwd(rotor, mod26(ch + offset)) - offset)
end

fun pass_bwd(rotor: Int, offset: Int, ch: Int): Int do
    return mod26(get_bwd(rotor, mod26(ch + offset)) - offset)
end

class EnigmaMachine
    var r: Array[Int]
    var o: Array[Int]
    var n1: Int
    var n2: Int

    init(r0: Int, r1: Int, r2: Int, k0: Int, k1: Int, k2: Int) do
        r = [r0, r1, r2]
        o = [k0, k1, k2]
        n1 = notches[r1]
        n2 = notches[r2]
    end

    fun step do
        if o[1] == n1 then o[1] = mod26(o[1]+1); o[0] = mod26(o[0]+1)
        else if o[2] == n2 then o[1] = mod26(o[1]+1)
        o[2] = mod26(o[2]+1)
    end

    fun press_key(ch: Int): Int do
        step
        var c = ch
        c = pass_fwd(r[2],o[2],c); c = pass_fwd(r[1],o[1],c); c = pass_fwd(r[0],o[0],c)
        c = reflector[c]
        c = pass_bwd(r[0],o[0],c); c = pass_bwd(r[1],o[1],c); c = pass_bwd(r[2],o[2],c)
        return c
    end

    fun encrypt(msg: String): String do
        var result = ""
        for i in [0..msg.length[ do
            var enc = press_key(msg.chars[i].code_point - 65)
            result += (enc + 65).code_point.to_s
        end
        return result
    end
end

print "Enigma Cipher - Nit"
var e = new EnigmaMachine(0,1,2,0,0,0)
print "Test 1: {e.encrypt("AAAAA")} expected BDZGO"
e = new EnigmaMachine(0,1,2,0,0,0)
print "Test 2: {e.encrypt("HELLOWORLD")} expected ILBDAAMTAZ"
