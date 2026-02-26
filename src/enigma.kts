// Enigma Cipher - Kotlin Script (.kts)
// Kotlin scripting mode (distinct from enigma.kt compiled Kotlin)
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

val fwd = arrayOf(
    intArrayOf(4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9),
    intArrayOf(0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4),
    intArrayOf(1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14))
val bwd = arrayOf(
    intArrayOf(20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9),
    intArrayOf(0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18),
    intArrayOf(19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12))
val ref = intArrayOf(24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19)
val notch = intArrayOf(16, 4, 21)

fun mod26(n: Int) = ((n % 26) + 26) % 26

data class Enigma(val r: IntArray, var o: IntArray, val n: IntArray, val pb: IntArray = IntArray(26) { it }) {
    fun step() {
        if (o[1] == n[1]) { o[1] = mod26(o[1]+1); o[0] = mod26(o[0]+1) }
        else if (o[2] == n[2]) { o[1] = mod26(o[1]+1) }
        o[2] = mod26(o[2]+1)
    }
    fun fwdPass(slot: Int, idx: Int): Int { val c = mod26(idx+o[slot]); return mod26(fwd[r[slot]][c]-o[slot]) }
    fun bwdPass(slot: Int, idx: Int): Int { val c = mod26(idx+o[slot]); return mod26(bwd[r[slot]][c]-o[slot]) }
    fun pressKey(ch: Int): Int {
        step()
        var c = pb[ch]
        c = fwdPass(2,c); c = fwdPass(1,c); c = fwdPass(0,c)
        c = ref[c]
        c = bwdPass(0,c); c = bwdPass(1,c); c = bwdPass(2,c)
        return pb[c]
    }
    fun encrypt(msg: String) = msg.uppercase().filter { it in 'A'..'Z' }.map { (pressKey(it-'A') + 65).toChar() }.joinToString("")
}

fun makeE(r1:Int,r2:Int,r3:Int,k1:Int,k2:Int,k3:Int) =
    Enigma(intArrayOf(r1-1,r2-1,r3-1), intArrayOf(k1,k2,k3), intArrayOf(notch[r1-1],notch[r2-1],notch[r3-1]))

println("Enigma Cipher - Kotlin Script")
data class T(val r:Triple<Int,Int,Int>, val k:Triple<Int,Int,Int>, val msg:String, val exp:String)
listOf(
    T(Triple(1,2,3),Triple(0,0,0),"AAAAA","BDZGO"),
    T(Triple(1,2,3),Triple(0,0,0),"HELLOWORLD","ILBDAAMTAZ"),
    T(Triple(1,2,3),Triple(0,0,0),"ATTACKATDAWN","BZHGNOCRRTCM"),
    T(Triple(1,2,3),Triple(12,2,10),"HELLOWORLD","DLTBBQVPQV"),
    T(Triple(3,1,2),Triple(0,0,0),"HELLOWORLD","KZHDFQYHXT"),
).forEachIndexed { i, t ->
    val e = makeE(t.r.first,t.r.second,t.r.third, t.k.first,t.k.second,t.k.third)
    val res = e.encrypt(t.msg)
    println("Test ${i+1}: $res ${if(res==t.exp)"[PASS]" else "[FAIL] expected ${t.exp}"}")
}
