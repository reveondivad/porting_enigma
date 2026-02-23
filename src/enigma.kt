// Enigma Machine — Rosetta Code Reference (Kotlin)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026

val FWD = arrayOf("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO")
val BWD = arrayOf("UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM")
val NOTCH = charArrayOf('Q','E','V')
val REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

class Enigma(rotors: IntArray, key: String, plugboard: List<String> = emptyList()) {
    private val id = rotors
    private val off = intArrayOf(key[0]-'A', key[1]-'A', key[2]-'A')
    private val plug = IntArray(26) { it }.also { p ->
        plugboard.forEach { s -> val a=s[0]-'A'; val b=s[1]-'A'; p[a]=b; p[b]=a }
    }

    private fun fwd(r:Int,i:Int):Int { val c=(i+off[r])%26; return(FWD[id[r]][c]-'A'-off[r]+26)%26 }
    private fun bwd(r:Int,i:Int):Int { val c=(i+off[r])%26; return(BWD[id[r]][c]-'A'-off[r]+26)%26 }

    private fun step() {
        if (off[1]+'A'.code == NOTCH[id[1]].code) { off[1]=(off[1]+1)%26; off[0]=(off[0]+1)%26 }
        else if (off[2]+'A'.code == NOTCH[id[2]].code) { off[1]=(off[1]+1)%26 }
        off[2]=(off[2]+1)%26
    }

    fun pressKey(ch: Char): Char {
        step(); var i=ch-'A'
        i=plug[i]
        i=fwd(2,i); i=fwd(1,i); i=fwd(0,i)
        i=REFLECTOR[i]-'A'
        i=bwd(0,i); i=bwd(1,i); i=bwd(2,i)
        i=plug[i]
        return 'A'+i
    }

    fun encrypt(text:String):String = text.uppercase().filter{it in 'A'..'Z'}.map{pressKey(it)}.joinToString("")
}

fun main() {
    data class TV(val r:IntArray, val k:String, val p:List<String>, val pt:String, val exp:String)
    val tests = listOf(
        TV(intArrayOf(0,1,2),"AAA",emptyList(),"AAAAA","BDZGO"),
        TV(intArrayOf(0,1,2),"AAA",emptyList(),"HELLOWORLD","ILBDAAMTAZ"),
        TV(intArrayOf(0,1,2),"AAA",emptyList(),"ATTACKATDAWN","BZHGNOCRRTCM"),
        TV(intArrayOf(0,1,2),"MCK",emptyList(),"HELLOWORLD","DLTBBQVPQV"),
        TV(intArrayOf(2,0,1),"AAA",emptyList(),"HELLOWORLD","KZHDFQYHXT"),
        TV(intArrayOf(0,1,2),"AAA",listOf("AB","CD","EF"),"HELLOWORLD","IKACBBMTBF"),
    )
    var allOk = true
    tests.forEachIndexed { t, tv ->
        val ct = Enigma(tv.r, tv.k, tv.p).encrypt(tv.pt)
        val ok = ct == tv.exp
        println("Test ${t+1}: ${tv.pt.padEnd(20)} -> ${ct.padEnd(15)} [${if(ok)"PASS" else "FAIL"}]")
        if (!ok) allOk = false
    }
    println(if (allOk) "\nALL 6 TESTS PASSED" else "\nSOME TESTS FAILED")
}
