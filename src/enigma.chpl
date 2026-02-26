// Enigma Machine - Chapel Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

const FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ",
              "AJDKSIRUXBLHWTMCQGZNPYFVOE",
              "BDFHJLCPRTXVZNYEIWGAKMUSQO"];
const BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ",
              "AJPCZWRLFBDKOTYUQGENHXMIVS",
              "TAGBPCSDQEUFVNZHYIXJWLRKOM"];
const NOTCH = [16, 4, 21];
const REFL = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

proc mod26(a: int): int { return ((a % 26) + 26) % 26; }
proc c2i(c: uint(8)): int { return c:int - 65; }
proc i2c(i: int): uint(8) { return (i + 65):uint(8); }

record Rotor {
    var fwd: string;
    var bwd: string;
    var notch: int;
    var offset: int;
}

proc makeRotor(num: int, win: uint(8)): Rotor {
    return new Rotor(FWD[num], BWD[num], NOTCH[num], c2i(win));
}

proc ref Rotor.forward(idx: int): int {
    var contact = mod26(idx + this.offset);
    return mod26(c2i(this.fwd.byte(contact)) - this.offset);
}

proc ref Rotor.backward(idx: int): int {
    var contact = mod26(idx + this.offset);
    return mod26(c2i(this.bwd.byte(contact)) - this.offset);
}

proc ref Rotor.step() { this.offset = (this.offset + 1) % 26; }
proc Rotor.atNotch(): bool { return this.offset == this.notch; }

record Enigma {
    var left: Rotor;
    var middle: Rotor;
    var right: Rotor;
    var plug: [0..25] int;
}

proc makeEnigma(rotors: [0..2] int, key: string, plugboard: [] string): Enigma {
    var e: Enigma;
    e.left = makeRotor(rotors[0], key.byte(0));
    e.middle = makeRotor(rotors[1], key.byte(1));
    e.right = makeRotor(rotors[2], key.byte(2));
    for i in 0..25 do e.plug[i] = i;
    for pair in plugboard {
        var a = c2i(pair.byte(0));
        var b = c2i(pair.byte(1));
        e.plug[a] = b;
        e.plug[b] = a;
    }
    return e;
}

proc ref Enigma.stepRotors() {
    if this.middle.atNotch() { this.middle.step(); this.left.step(); }
    else if this.right.atNotch() { this.middle.step(); }
    this.right.step();
}

proc ref Enigma.pressKey(c: uint(8)): uint(8) {
    this.stepRotors();
    var idx = c2i(c);
    idx = this.plug[idx];
    idx = this.right.forward(idx);
    idx = this.middle.forward(idx);
    idx = this.left.forward(idx);
    idx = c2i(REFL.byte(idx));
    idx = this.left.backward(idx);
    idx = this.middle.backward(idx);
    idx = this.right.backward(idx);
    idx = this.plug[idx];
    return i2c(idx);
}

proc ref Enigma.encrypt(text: string): string {
    var result: string;
    var upper = text.toUpper();
    for i in 0..#upper.size {
        var c = upper.byte(i);
        if c >= 65 && c <= 90 then
            result += this.pressKey(c):string;
    }
    return result;
}

proc main() {
    writeln("Enigma Machine - Chapel Implementation");
    writeln("=======================================");

    var allPass = true;

    // Test cases inline (Chapel doesn't have easy tuple arrays)
    var r1: [0..2] int = [0, 1, 2]; var k1 = "AAA"; var p1: [0..-1] string;
    var e1 = makeEnigma(r1, k1, p1);
    var c1 = e1.encrypt("AAAAA");
    var ok1 = c1 == "BDZGO";
    writef("  Test 1: %-20s -> %-15s [%s]\n", "AAAAA", c1, if ok1 then "PASS" else "FAIL");
    if !ok1 { writeln("          Expected BDZGO"); allPass = false; }

    var e2 = makeEnigma(r1, k1, p1);
    var c2 = e2.encrypt("HELLOWORLD");
    var ok2 = c2 == "ILBDAAMTAZ";
    writef("  Test 2: %-20s -> %-15s [%s]\n", "HELLOWORLD", c2, if ok2 then "PASS" else "FAIL");
    if !ok2 { writeln("          Expected ILBDAAMTAZ"); allPass = false; }

    var e3 = makeEnigma(r1, k1, p1);
    var c3 = e3.encrypt("ATTACKATDAWN");
    var ok3 = c3 == "BZHGNOCRRTCM";
    writef("  Test 3: %-20s -> %-15s [%s]\n", "ATTACKATDAWN", c3, if ok3 then "PASS" else "FAIL");
    if !ok3 { writeln("          Expected BZHGNOCRRTCM"); allPass = false; }

    var e4 = makeEnigma(r1, "MCK", p1);
    var c4 = e4.encrypt("HELLOWORLD");
    var ok4 = c4 == "DLTBBQVPQV";
    writef("  Test 4: %-20s -> %-15s [%s]\n", "HELLOWORLD", c4, if ok4 then "PASS" else "FAIL");
    if !ok4 { writeln("          Expected DLTBBQVPQV"); allPass = false; }

    var r5: [0..2] int = [2, 0, 1];
    var e5 = makeEnigma(r5, k1, p1);
    var c5 = e5.encrypt("HELLOWORLD");
    var ok5 = c5 == "KZHDFQYHXT";
    writef("  Test 5: %-20s -> %-15s [%s]\n", "HELLOWORLD", c5, if ok5 then "PASS" else "FAIL");
    if !ok5 { writeln("          Expected KZHDFQYHXT"); allPass = false; }

    var p6: [0..2] string = ["AB", "CD", "EF"];
    var e6 = makeEnigma(r1, k1, p6);
    var c6 = e6.encrypt("HELLOWORLD");
    var ok6 = c6 == "IKACBBMTBF";
    writef("  Test 6: %-20s -> %-15s [%s]\n", "HELLOWORLD", c6, if ok6 then "PASS" else "FAIL");
    if !ok6 { writeln("          Expected IKACBBMTBF"); allPass = false; }

    writeln(if allPass then "\n  ALL 6 TESTS PASSED" else "\n  SOME TESTS FAILED");
}
