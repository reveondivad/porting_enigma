// Enigma Machine — Rosetta Code Reference (TypeScript)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026

const FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"];
const BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"];
const NOTCH = ["Q","E","V"];
const REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

class Enigma {
  private id: number[];
  private off: number[];
  private plug: number[];

  constructor(rotors: number[], key: string, plugboard: string[] = []) {
    this.id = rotors;
    this.off = [...key].map(c => c.charCodeAt(0) - 65);
    this.plug = Array.from({length: 26}, (_, i) => i);
    for (const p of plugboard) {
      const [a, b] = [p.charCodeAt(0) - 65, p.charCodeAt(1) - 65];
      this.plug[a] = b; this.plug[b] = a;
    }
  }

  private fwd(r: number, i: number): number {
    const c = (i + this.off[r]) % 26;
    return (FWD[this.id[r]].charCodeAt(c) - 65 - this.off[r] + 26) % 26;
  }
  private bwd(r: number, i: number): number {
    const c = (i + this.off[r]) % 26;
    return (BWD[this.id[r]].charCodeAt(c) - 65 - this.off[r] + 26) % 26;
  }

  private step(): void {
    if (this.off[1] + 65 === NOTCH[this.id[1]].charCodeAt(0)) {
      this.off[1] = (this.off[1] + 1) % 26;
      this.off[0] = (this.off[0] + 1) % 26;
    } else if (this.off[2] + 65 === NOTCH[this.id[2]].charCodeAt(0)) {
      this.off[1] = (this.off[1] + 1) % 26;
    }
    this.off[2] = (this.off[2] + 1) % 26;
  }

  pressKey(ch: string): string {
    this.step();
    let i = ch.charCodeAt(0) - 65;
    i = this.plug[i];
    i = this.fwd(2, i); i = this.fwd(1, i); i = this.fwd(0, i);
    i = REFLECTOR.charCodeAt(i) - 65;
    i = this.bwd(0, i); i = this.bwd(1, i); i = this.bwd(2, i);
    i = this.plug[i];
    return String.fromCharCode(i + 65);
  }

  encrypt(text: string): string {
    return text.toUpperCase().replace(/[^A-Z]/g, "").split("").map(c => this.pressKey(c)).join("");
  }
}

// Test vectors
const tests: [number[], string, string[], string, string][] = [
  [[0,1,2], "AAA", [],            "AAAAA",        "BDZGO"],
  [[0,1,2], "AAA", [],            "HELLOWORLD",   "ILBDAAMTAZ"],
  [[0,1,2], "AAA", [],            "ATTACKATDAWN", "BZHGNOCRRTCM"],
  [[0,1,2], "MCK", [],            "HELLOWORLD",   "DLTBBQVPQV"],
  [[2,0,1], "AAA", [],            "HELLOWORLD",   "KZHDFQYHXT"],
  [[0,1,2], "AAA", ["AB","CD","EF"],"HELLOWORLD", "IKACBBMTBF"],
];

let allOk = true;
tests.forEach(([r,k,p,pt,exp], t) => {
  const ct = new Enigma(r,k,p).encrypt(pt);
  const ok = ct === exp;
  console.log(`Test ${t+1}: ${pt.padEnd(20)} -> ${ct.padEnd(15)} [${ok?"PASS":"FAIL"}]`);
  if (!ok) allOk = false;
});
console.log(allOk ? "\nALL 6 TESTS PASSED" : "\nSOME TESTS FAILED");
