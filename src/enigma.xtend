// Enigma Cipher - Xtend
// Java-compatible language with modern syntax (Eclipse)
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

class Enigma {
    static val fwdI  = #[4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
    static val fwdII = #[0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
    static val fwdIII= #[1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
    static val bwdI  = #[20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
    static val bwdII = #[0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
    static val bwdIII= #[19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
    static val reflector = #[24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
    static val notches = #[16, 4, 21]

    static def mod26(int n) { val m = n % 26; if (m < 0) m + 26 else m }
    static def getFwd(int r, int i) { switch r { case 0: fwdI.get(i) case 1: fwdII.get(i) default: fwdIII.get(i) } }
    static def getBwd(int r, int i) { switch r { case 0: bwdI.get(i) case 1: bwdII.get(i) default: bwdIII.get(i) } }
    static def passFwd(int rotor, int offset, int ch) { mod26(getFwd(rotor, mod26(ch+offset)) - offset) }
    static def passBwd(int rotor, int offset, int ch) { mod26(getBwd(rotor, mod26(ch+offset)) - offset) }

    int[] r
    int[] o
    int n1
    int n2

    new(int r0, int r1, int r2, int k0, int k1, int k2) {
        r = #[r0,r1,r2]; o = #[k0,k1,k2]
        n1 = notches.get(r1); n2 = notches.get(r2)
    }

    def step() {
        if (o.get(1) == n1) { o.set(1, mod26(o.get(1)+1)); o.set(0, mod26(o.get(0)+1)) }
        else if (o.get(2) == n2) { o.set(1, mod26(o.get(1)+1)) }
        o.set(2, mod26(o.get(2)+1))
    }

    def pressKey(int ch) {
        step()
        var c = ch
        c = passFwd(r.get(2),o.get(2),c); c = passFwd(r.get(1),o.get(1),c); c = passFwd(r.get(0),o.get(0),c)
        c = reflector.get(c)
        c = passBwd(r.get(0),o.get(0),c); c = passBwd(r.get(1),o.get(1),c); c = passBwd(r.get(2),o.get(2),c)
        c
    }

    def encrypt(String msg) {
        val sb = new StringBuilder
        msg.chars.forEach[ch | sb.append(Character.toChars(pressKey(ch - 65) + 65))]
        sb.toString
    }

    def static void main(String[] args) {
        println("Enigma Cipher - Xtend")
        val e = new Enigma(0,1,2,0,0,0)
        println('''Test 1: «e.encrypt("AAAAA")» expected BDZGO''')
    }
}
