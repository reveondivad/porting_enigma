// Enigma Machine — Rosetta Code Reference (JavaScript)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026

const ALPHA = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"];
const BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"];
const NOTCH = ["Q","E","V"];
const REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

class Rotor {
  constructor(id, window) {
    this.id = id;
    this.notch = NOTCH[id];
    this.offset = ALPHA.indexOf(window);
  }
  atNotch() { return ALPHA[this.offset] === this.notch; }
  step() { this.offset = (this.offset + 1) % 26; }
  fwd(i) {
    const c = (i + this.offset) % 26;
    return (ALPHA.indexOf(FWD[this.id][c]) - this.offset + 26) % 26;
  }
  bwd(i) {
    const c = (i + this.offset) % 26;
    return (ALPHA.indexOf(BWD[this.id][c]) - this.offset + 26) % 26;
  }
}

class Enigma {
  constructor(rotors, key, plugboard) {
    this.L = new Rotor(rotors[0], key[0]);
    this.M = new Rotor(rotors[1], key[1]);
    this.R = new Rotor(rotors[2], key[2]);
    this.plug = Array.from({length:26}, (_,i) => i);
    if (plugboard) {
      for (const p of plugboard) {
        const a = ALPHA.indexOf(p[0]), b = ALPHA.indexOf(p[1]);
        this.plug[a] = b; this.plug[b] = a;
      }
    }
  }
  _step() {
    if (this.M.atNotch()) { this.M.step(); this.L.step(); }
    else if (this.R.atNotch()) { this.M.step(); }
    this.R.step();
  }
  pressKey(ch) {
    this._step();
    let i = ALPHA.indexOf(ch);
    i = this.plug[i];
    i = this.R.fwd(i); i = this.M.fwd(i); i = this.L.fwd(i);
    i = ALPHA.indexOf(REFLECTOR[i]);
    i = this.L.bwd(i); i = this.M.bwd(i); i = this.R.bwd(i);
    i = this.plug[i];
    return ALPHA[i];
  }
  encrypt(text) {
    return text.toUpperCase().replace(/[^A-Z]/g,"").split("").map(c => this.pressKey(c)).join("");
  }
}

// Test vectors
const tests = [
  [[0,1,2], "AAA", null,           "AAAAA",        "BDZGO"],
  [[0,1,2], "AAA", null,           "HELLOWORLD",   "ILBDAAMTAZ"],
  [[0,1,2], "AAA", null,           "ATTACKATDAWN", "BZHGNOCRRTCM"],
  [[0,1,2], "MCK", null,           "HELLOWORLD",   "DLTBBQVPQV"],
  [[2,0,1], "AAA", null,           "HELLOWORLD",   "KZHDFQYHXT"],
  [[0,1,2], "AAA", ["AB","CD","EF"],"HELLOWORLD",  "IKACBBMTBF"],
];

let allOk = true;
tests.forEach(([r,k,p,plain,exp], i) => {
  const ct = new Enigma(r,k,p).encrypt(plain);
  const ok = ct === exp;
  console.log(`Test ${i+1}: ${plain.padEnd(20)} -> ${ct.padEnd(15)} [${ok?"PASS":"FAIL"}]`);
  if (!ok) allOk = false;
});
console.log(allOk ? "\nALL 6 TESTS PASSED" : "\nSOME TESTS FAILED");
