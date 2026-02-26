// Enigma Machine - ActionScript 3 Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

package {
    import flash.display.Sprite;

    public class Enigma extends Sprite {
        private static const FWD:Array = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ",
            "AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"];
        private static const BWD:Array = ["UWYGADFPVZBECKMTHXSLRINQOJ",
            "AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"];
        private static const NOTCH:Array = [16, 4, 21];
        private static const REFL:String = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

        private var leftFwd:String, midFwd:String, rightFwd:String;
        private var leftBwd:String, midBwd:String, rightBwd:String;
        private var leftNotch:int, midNotch:int, rightNotch:int;
        private var leftOff:int, midOff:int, rightOff:int;
        private var plug:Array;

        private static function mod26(a:int):int {
            return ((a % 26) + 26) % 26;
        }

        public function Enigma() {
            trace("Enigma Machine - ActionScript 3 Implementation");
            trace("===============================================");
            runTests();
        }

        private function init(rotors:Array, key:String, plugboard:Array):void {
            leftFwd = FWD[rotors[0]]; leftBwd = BWD[rotors[0]]; leftNotch = NOTCH[rotors[0]];
            midFwd = FWD[rotors[1]]; midBwd = BWD[rotors[1]]; midNotch = NOTCH[rotors[1]];
            rightFwd = FWD[rotors[2]]; rightBwd = BWD[rotors[2]]; rightNotch = NOTCH[rotors[2]];
            leftOff = key.charCodeAt(0) - 65;
            midOff = key.charCodeAt(1) - 65;
            rightOff = key.charCodeAt(2) - 65;
            plug = new Array(26);
            for (var i:int = 0; i < 26; i++) plug[i] = i;
            if (plugboard) {
                for each (var pair:String in plugboard) {
                    var a:int = pair.charCodeAt(0) - 65;
                    var b:int = pair.charCodeAt(1) - 65;
                    plug[a] = b; plug[b] = a;
                }
            }
        }

        private function fwdPass(wiring:String, off:int, idx:int):int {
            var contact:int = mod26(idx + off);
            return mod26(wiring.charCodeAt(contact) - 65 - off);
        }

        private function bwdPass(wiring:String, off:int, idx:int):int {
            var contact:int = mod26(idx + off);
            return mod26(wiring.charCodeAt(contact) - 65 - off);
        }

        private function stepRotors():void {
            if (midOff == midNotch) { midOff = (midOff + 1) % 26; leftOff = (leftOff + 1) % 26; }
            else if (rightOff == rightNotch) { midOff = (midOff + 1) % 26; }
            rightOff = (rightOff + 1) % 26;
        }

        private function pressKey(c:int):int {
            stepRotors();
            var idx:int = c - 65;
            idx = plug[idx];
            idx = fwdPass(rightFwd, rightOff, idx);
            idx = fwdPass(midFwd, midOff, idx);
            idx = fwdPass(leftFwd, leftOff, idx);
            idx = REFL.charCodeAt(idx) - 65;
            idx = bwdPass(leftBwd, leftOff, idx);
            idx = bwdPass(midBwd, midOff, idx);
            idx = bwdPass(rightBwd, rightOff, idx);
            idx = plug[idx];
            return idx + 65;
        }

        private function encrypt(text:String):String {
            var result:String = "";
            var upper:String = text.toUpperCase();
            for (var i:int = 0; i < upper.length; i++) {
                var c:int = upper.charCodeAt(i);
                if (c >= 65 && c <= 90) result += String.fromCharCode(pressKey(c));
            }
            return result;
        }

        private function runTests():void {
            var tests:Array = [
                [[0,1,2], "AAA", null,              "AAAAA",        "BDZGO"],
                [[0,1,2], "AAA", null,              "HELLOWORLD",   "ILBDAAMTAZ"],
                [[0,1,2], "AAA", null,              "ATTACKATDAWN", "BZHGNOCRRTCM"],
                [[0,1,2], "MCK", null,              "HELLOWORLD",   "DLTBBQVPQV"],
                [[2,0,1], "AAA", null,              "HELLOWORLD",   "KZHDFQYHXT"],
                [[0,1,2], "AAA", ["AB","CD","EF"],  "HELLOWORLD",   "IKACBBMTBF"]
            ];

            var allPass:Boolean = true;
            for (var i:int = 0; i < tests.length; i++) {
                var t:Array = tests[i];
                init(t[0], t[1], t[2]);
                var cipher:String = encrypt(t[3]);
                var ok:Boolean = (cipher == t[4]);
                var status:String = ok ? "PASS" : "FAIL";
                trace("  Test " + (i+1) + ": " + t[3] + " -> " + cipher + " [" + status + "]");
                if (!ok) { trace("          Expected " + t[4]); allPass = false; }
            }
            trace(allPass ? "\n  ALL 6 TESTS PASSED" : "\n  SOME TESTS FAILED");
        }
    }
}
