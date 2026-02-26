// Enigma cipher in ECMAScript 2024 (latest JS standard with decorators)
const ROTOR_FWD = Object.freeze([
  Object.freeze([4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]),
  Object.freeze([0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]),
  Object.freeze([1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]),
]);
const ROTOR_BWD = Object.freeze([
  Object.freeze([20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]),
  Object.freeze([0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]),
  Object.freeze([19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]),
]);
const REFLECTOR = Object.freeze([24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]);
const NOTCHES = Object.freeze([16, 4, 21]);

const mod26 = (n) => ((n % 26) + 26) % 26;

class EnigmaMachine {
  #pos = [0, 0, 0];
  
  #rotorPass(wiring, c, pos) {
    return mod26(wiring[mod26(c + pos)] - pos);
  }

  encrypt(text) {
    this.#pos = [0, 0, 0];
    return [...text.toUpperCase()].reduce((result, ch) => {
      let c = ch.charCodeAt(0) - 65;
      if (c < 0 || c > 25) return result;
      const mid = this.#pos[1] === NOTCHES[1];
      if (this.#pos[2] === NOTCHES[2]) this.#pos[2] = mod26(this.#pos[2] + 1);
      if (mid || this.#pos[2] === NOTCHES[2]) this.#pos[1] = mod26(this.#pos[1] + 1);
      this.#pos[2] = mod26(this.#pos[2] + 1);
      for (let i = 2; i >= 0; i--) c = this.#rotorPass(ROTOR_FWD[i], c, this.#pos[i]);
      c = REFLECTOR[c];
      for (let i = 0; i <= 2; i++) c = this.#rotorPass(ROTOR_BWD[i], c, this.#pos[i]);
      return result + String.fromCharCode(c + 65);
    }, "");
  }
}

using(machine = new EnigmaMachine()) {
  console.log(machine.encrypt("HELLOWORLD"));
}
