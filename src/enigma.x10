// Enigma Cipher - X10
// Parallel programming language from IBM Research
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

public class Enigma {
    static val FWD = [
        [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9] as Rail[Int],
        [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4] as Rail[Int],
        [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14] as Rail[Int]
    ];
    static val BWD = [
        [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9] as Rail[Int],
        [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18] as Rail[Int],
        [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12] as Rail[Int]
    ];
    static val REF = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19] as Rail[Int];
    static val NOTCH = [16n, 4n, 21n] as Rail[Int];

    var rotors:Rail[Int];
    var offsets:Rail[Int];
    var notches:Rail[Int];
    var plugboard:Rail[Int];

    def this(r1:Int, r2:Int, r3:Int, k1:Int, k2:Int, k3:Int) {
        rotors = [r1-1, r2-1, r3-1] as Rail[Int];
        offsets = [k1, k2, k3] as Rail[Int];
        notches = [NOTCH(rotors(0)), NOTCH(rotors(1)), NOTCH(rotors(2))] as Rail[Int];
        plugboard = new Rail[Int](26, (i:Long) => i as Int);
    }

    static def mod26(n:Int):Int = ((n % 26) + 26) % 26;

    def step():void {
        if (offsets(1) == notches(1)) {
            offsets(1) = mod26(offsets(1) + 1);
            offsets(0) = mod26(offsets(0) + 1);
        } else if (offsets(2) == notches(2)) {
            offsets(1) = mod26(offsets(1) + 1);
        }
        offsets(2) = mod26(offsets(2) + 1);
    }

    def fwdPass(rotor:Int, idx:Int):Int {
        val contact = mod26(idx + offsets(rotor));
        val out = FWD(rotors(rotor))(contact);
        return mod26(out - offsets(rotor));
    }

    def bwdPass(rotor:Int, idx:Int):Int {
        val contact = mod26(idx + offsets(rotor));
        val out = BWD(rotors(rotor))(contact);
        return mod26(out - offsets(rotor));
    }

    def pressKey(c:Char):Char {
        step();
        var idx:Int = plugboard(c.ord() - 65n);
        idx = fwdPass(2, idx); idx = fwdPass(1, idx); idx = fwdPass(0, idx);
        idx = REF(idx);
        idx = bwdPass(0, idx); idx = bwdPass(1, idx); idx = bwdPass(2, idx);
        idx = plugboard(idx);
        return (65 + idx) as Char;
    }

    def encrypt(text:String):String {
        val sb = new x10.util.StringBuilder();
        for (i in 0..(text.length()-1)) {
            val c = text.charAt(i).toUpperCase();
            if (c >= 'A' && c <= 'Z') sb.add(pressKey(c));
        }
        return sb.toString();
    }

    public static def main(args:Rail[String]):void {
        Console.OUT.println("Enigma Cipher - X10");
        val tests = [
            [1,2,3,0,0,0,"AAAAA","BDZGO"],
            [1,2,3,0,0,0,"HELLOWORLD","ILBDAAMTAZ"],
            [1,2,3,0,0,0,"ATTACKATDAWN","BZHGNOCRRTCM"],
            [1,2,3,12,2,10,"HELLOWORLD","DLTBBQVPQV"],
            [3,1,2,0,0,0,"HELLOWORLD","KZHDFQYHXT"],
        ];
        for (t in tests.range()) {
            val e = new Enigma(tests(t)(0) as Int, tests(t)(1) as Int, tests(t)(2) as Int,
                               tests(t)(3) as Int, tests(t)(4) as Int, tests(t)(5) as Int);
            val result = e.encrypt(tests(t)(6) as String);
            val ok = result.equals(tests(t)(7) as String);
            Console.OUT.println("Test " + (t+1) + ": " + result + (ok ? " [PASS]" : " [FAIL]"));
        }
    }
}
