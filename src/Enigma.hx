// Enigma Machine - Haxe Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

class Enigma {
    static var FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"];
    static var BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"];
    static var REF = "YRUHQSLDPXNGOKMIEBFZCWVJAT";
    static var NOTCH = [16, 4, 21];

    var rotors:Array<Int>;
    var offsets:Array<Int>;
    var notches:Array<Int>;
    var plugboard:Array<Int>;

    public function new(r1:Int, r2:Int, r3:Int, k1:String, k2:String, k3:String, plugPairs:String = "") {
        rotors = [r1-1, r2-1, r3-1];
        offsets = [k1.charCodeAt(0)-65, k2.charCodeAt(0)-65, k3.charCodeAt(0)-65];
        notches = [NOTCH[rotors[0]], NOTCH[rotors[1]], NOTCH[rotors[2]]];
        plugboard = [for (i in 0...26) i];
        if (plugPairs.length > 0) {
            for (pair in plugPairs.split("-")) {
                var a = pair.charCodeAt(0) - 65;
                var b = pair.charCodeAt(1) - 65;
                plugboard[a] = b;
                plugboard[b] = a;
            }
        }
    }

    static function mod26(n:Int):Int {
        return ((n % 26) + 26) % 26;
    }

    function step():Void {
        if (offsets[1] == notches[1]) {
            offsets[1] = mod26(offsets[1] + 1);
            offsets[0] = mod26(offsets[0] + 1);
        } else if (offsets[2] == notches[2]) {
            offsets[1] = mod26(offsets[1] + 1);
        }
        offsets[2] = mod26(offsets[2] + 1);
    }

    function fwdPass(rotor:Int, idx:Int):Int {
        var contact = mod26(idx + offsets[rotor]);
        var out = FWD[rotors[rotor]].charCodeAt(contact) - 65;
        return mod26(out - offsets[rotor]);
    }

    function bwdPass(rotor:Int, idx:Int):Int {
        var contact = mod26(idx + offsets[rotor]);
        var out = BWD[rotors[rotor]].charCodeAt(contact) - 65;
        return mod26(out - offsets[rotor]);
    }

    public function pressKey(c:Int):Int {
        step();
        var idx = plugboard[c - 65];
        idx = fwdPass(2, idx);
        idx = fwdPass(1, idx);
        idx = fwdPass(0, idx);
        idx = REF.charCodeAt(idx) - 65;
        idx = bwdPass(0, idx);
        idx = bwdPass(1, idx);
        idx = bwdPass(2, idx);
        idx = plugboard[idx];
        return 65 + idx;
    }

    public function encrypt(text:String):String {
        var result = new StringBuf();
        var upper = text.toUpperCase();
        for (i in 0...upper.length) {
            var c = upper.charCodeAt(i);
            if (c >= 65 && c <= 90) {
                result.addChar(pressKey(c));
            }
        }
        return result.toString();
    }

    static function main() {
        Sys.println("Enigma Machine - Haxe Implementation");
        Sys.println("=====================================");

        var tests:Array<{rotors:Array<Int>, key:String, plug:String, input:String, expected:String}> = [
            {rotors:[1,2,3], key:"AAA", plug:"", input:"AAAAA", expected:"BDZGO"},
            {rotors:[1,2,3], key:"AAA", plug:"", input:"HELLOWORLD", expected:"ILBDAAMTAZ"},
            {rotors:[1,2,3], key:"AAA", plug:"", input:"ATTACKATDAWN", expected:"BZHGNOCRRTCM"},
            {rotors:[1,2,3], key:"MCK", plug:"", input:"HELLOWORLD", expected:"DLTBBQVPQV"},
            {rotors:[3,1,2], key:"AAA", plug:"", input:"HELLOWORLD", expected:"KZHDFQYHXT"},
            {rotors:[1,2,3], key:"AAA", plug:"AB-CD-EF", input:"HELLOWORLD", expected:"IKACBBMTBF"}
        ];

        var pass = 0;
        for (i in 0...tests.length) {
            var t = tests[i];
            var e = new Enigma(t.rotors[0], t.rotors[1], t.rotors[2],
                String.fromCharCode(t.key.charCodeAt(0)),
                String.fromCharCode(t.key.charCodeAt(1)),
                String.fromCharCode(t.key.charCodeAt(2)), t.plug);
            var result = e.encrypt(t.input);
            var ok = result == t.expected;
            if (ok) pass++;
            Sys.println('Test ${i+1}: ${t.input} -> ${result} ${ok ? "[PASS]" : "[FAIL] expected " + t.expected}');
        }
        Sys.println('\n${pass}/6 tests passed');
    }
}
