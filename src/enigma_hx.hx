// Enigma Machine - Haxe Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026
// Note: Haxe compiles to multiple targets (JS, C++, Java, Python, etc.)

class Rotor {
    static var FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"];
    static var BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"];
    static var NOTCH = [16, 4, 21];

    public var fwd:String;
    public var bwd:String;
    public var notch:Int;
    public var offset:Int;

    public function new(num:Int, win:Int) {
        fwd = FWD[num]; bwd = BWD[num];
        notch = NOTCH[num]; offset = win - 65;
    }

    static function mod26(a:Int):Int { return ((a % 26) + 26) % 26; }

    public function forward(idx:Int):Int {
        var contact = mod26(idx + offset);
        return mod26(fwd.charCodeAt(contact) - 65 - offset);
    }
    public function backward(idx:Int):Int {
        var contact = mod26(idx + offset);
        return mod26(bwd.charCodeAt(contact) - 65 - offset);
    }
    public function step() { offset = (offset + 1) % 26; }
    public function atNotch():Bool { return offset == notch; }
}

class Enigma {
    var left:Rotor;
    var middle:Rotor;
    var right:Rotor;
    var plug:Array<Int>;

    public function new(rotors:Array<Int>, key:String, plugboard:Array<String>) {
        left = new Rotor(rotors[0], key.charCodeAt(0));
        middle = new Rotor(rotors[1], key.charCodeAt(1));
        right = new Rotor(rotors[2], key.charCodeAt(2));
        plug = [for (i in 0...26) i];
        if (plugboard != null) {
            for (pair in plugboard) {
                var a = pair.charCodeAt(0) - 65;
                var b = pair.charCodeAt(1) - 65;
                plug[a] = b; plug[b] = a;
            }
        }
    }

    function stepRotors() {
        if (middle.atNotch()) { middle.step(); left.step(); }
        else if (right.atNotch()) { middle.step(); }
        right.step();
    }

    function pressKey(c:Int):Int {
        stepRotors();
        var refl = "YRUHQSLDPXNGOKMIEBFZCWVJAT";
        var idx = c - 65;
        idx = plug[idx];
        idx = right.forward(idx);
        idx = middle.forward(idx);
        idx = left.forward(idx);
        idx = refl.charCodeAt(idx) - 65;
        idx = left.backward(idx);
        idx = middle.backward(idx);
        idx = right.backward(idx);
        idx = plug[idx];
        return idx + 65;
    }

    public function encrypt(text:String):String {
        var result = new StringBuf();
        var upper = text.toUpperCase();
        for (i in 0...upper.length) {
            var c = upper.charCodeAt(i);
            if (c >= 65 && c <= 90) result.addChar(pressKey(c));
        }
        return result.toString();
    }
}

class EnigmaMain {
    static function main() {
        Sys.println("Enigma Machine - Haxe Implementation");
        Sys.println("=====================================");

        var tests:Array<{rotors:Array<Int>, key:String, plugs:Array<String>, plain:String, expected:String}> = [
            {rotors:[0,1,2], key:"AAA", plugs:null,              plain:"AAAAA",        expected:"BDZGO"},
            {rotors:[0,1,2], key:"AAA", plugs:null,              plain:"HELLOWORLD",   expected:"ILBDAAMTAZ"},
            {rotors:[0,1,2], key:"AAA", plugs:null,              plain:"ATTACKATDAWN", expected:"BZHGNOCRRTCM"},
            {rotors:[0,1,2], key:"MCK", plugs:null,              plain:"HELLOWORLD",   expected:"DLTBBQVPQV"},
            {rotors:[2,0,1], key:"AAA", plugs:null,              plain:"HELLOWORLD",   expected:"KZHDFQYHXT"},
            {rotors:[0,1,2], key:"AAA", plugs:["AB","CD","EF"],  plain:"HELLOWORLD",   expected:"IKACBBMTBF"}
        ];

        var allPass = true;
        for (i in 0...tests.length) {
            var t = tests[i];
            var e = new Enigma(t.rotors, t.key, t.plugs);
            var cipher = e.encrypt(t.plain);
            var ok = cipher == t.expected;
            var status = ok ? "PASS" : "FAIL";
            Sys.println('  Test ${i+1}: ${t.plain} -> $cipher [$status]');
            if (!ok) { Sys.println('          Expected ${t.expected}'); allPass = false; }
        }
        Sys.println(allPass ? "\n  ALL 6 TESTS PASSED" : "\n  SOME TESTS FAILED");
    }
}
