#!/usr/bin/env python3
"""
Enigma Machine - Rosetta Code Reference Implementation
=======================================================
A self-contained Wehrmacht Enigma I (3-rotor, Reflector B, plugboard).
Historically accurate stepping including the double-step anomaly.

Based on Emily Willson's simulation (2018), corrected to match the
standard Enigma specification used by all reference simulators.

────────────────────────────────────────────────────────────────────
STANDARD TEST VECTORS — every port must reproduce these exactly:

 #  Rotors      Key  Plugboard    Plaintext       -> Ciphertext
 1  I  II  III  AAA  (none)       AAAAA           -> BDZGO
 2  I  II  III  AAA  (none)       HELLOWORLD      -> ILBDAAMTAZ
 3  I  II  III  AAA  (none)       ATTACKATDAWN    -> BZHGNOCRRTCM
 4  I  II  III  MCK  (none)       HELLOWORLD      -> DLTBBQVPQV
 5  III I  II   AAA  (none)       HELLOWORLD      -> KZHDFQYHXT
 6  I  II  III  AAA  AB CD EF    HELLOWORLD      -> IKACBBMTBF

Decryption == Encryption (Enigma is its own inverse).
────────────────────────────────────────────────────────────────────

PeopleTec Inc. — Guinness World Record Attempt 2026
"""

# ═══════════════════════════ CONSTANTS ════════════════════════════

ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#  Wehrmacht rotor wirings: index i maps letter ALPHABET[i] to the
#  letter at position i in the wiring string.
ROTOR_FORWARD = {
    "I":   "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
    "II":  "AJDKSIRUXBLHWTMCQGZNPYFVOE",
    "III": "BDFHJLCPRTXVZNYEIWGAKMUSQO",
}

ROTOR_BACKWARD = {
    "I":   "UWYGADFPVZBECKMTHXSLRINQOJ",
    "II":  "AJPCZWRLFBDKOTYUQGENHXMIVS",
    "III": "TAGBPCSDQEUFVNZHYIXJWLRKOM",
}

# Turnover notch: right/middle rotor causes its neighbor to step
# when this letter is showing in the window BEFORE stepping.
ROTOR_NOTCHES = {"I": "Q", "II": "E", "III": "V"}

# Reflector B (Wehrmacht standard)
REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT"


# ═══════════════════════════ ROTOR ═══════════════════════════════

class Rotor:
    def __init__(self, name, window):
        self.forward  = ROTOR_FORWARD[name]
        self.backward = ROTOR_BACKWARD[name]
        self.notch    = ROTOR_NOTCHES[name]
        self.offset   = ALPHABET.index(window)

    def at_notch(self):
        return ALPHABET[self.offset] == self.notch

    def step(self):
        self.offset = (self.offset + 1) % 26

    def forward_pass(self, idx):
        contact = (idx + self.offset) % 26
        out     = ALPHABET.index(self.forward[contact])
        return (out - self.offset) % 26

    def backward_pass(self, idx):
        contact = (idx + self.offset) % 26
        out     = ALPHABET.index(self.backward[contact])
        return (out - self.offset) % 26


# ═══════════════════════════ ENIGMA ══════════════════════════════

class Enigma:
    def __init__(self, rotors=("I", "II", "III"), key="AAA", plugboard=None):
        """
        rotors:    3 rotor names (left, middle, right)
        key:       3-letter starting window  e.g. "AAA"
        plugboard: list of 2-char swap strings e.g. ["AB", "CD"]
        """
        self.left   = Rotor(rotors[0], key[0])
        self.middle = Rotor(rotors[1], key[1])
        self.right  = Rotor(rotors[2], key[2])

        # Plugboard: build a 26-element substitution array
        self.plug = list(range(26))
        if plugboard:
            for pair in plugboard:
                a = ALPHABET.index(pair[0].upper())
                b = ALPHABET.index(pair[1].upper())
                self.plug[a] = b
                self.plug[b] = a

    def _step(self):
        """Step rotors with the double-stepping anomaly."""
        if self.middle.at_notch():
            self.middle.step()
            self.left.step()
        elif self.right.at_notch():
            self.middle.step()
        self.right.step()

    def press_key(self, char):
        """Encrypt one uppercase letter."""
        self._step()
        idx = ALPHABET.index(char)
        idx = self.plug[idx]                         # plugboard in
        idx = self.right.forward_pass(idx)            # R ->
        idx = self.middle.forward_pass(idx)           # M ->
        idx = self.left.forward_pass(idx)             # L ->
        idx = ALPHABET.index(REFLECTOR[idx])          # reflector
        idx = self.left.backward_pass(idx)            # <- L
        idx = self.middle.backward_pass(idx)          # <- M
        idx = self.right.backward_pass(idx)           # <- R
        idx = self.plug[idx]                         # plugboard out
        return ALPHABET[idx]

    def encrypt(self, text):
        """Encrypt a string (only A-Z kept, spaces/punctuation stripped)."""
        return "".join(self.press_key(c) for c in text.upper() if c in ALPHABET)

    # Enigma is its own inverse
    decrypt = encrypt


# ═══════════════════════════ VERIFICATION ════════════════════════

TEST_VECTORS = [
    # (rotors,              key,   plugboard,        plaintext,        expected)
    (("I","II","III"),      "AAA", None,             "AAAAA",          "BDZGO"),
    (("I","II","III"),      "AAA", None,             "HELLOWORLD",     "ILBDAAMTAZ"),
    (("I","II","III"),      "AAA", None,             "ATTACKATDAWN",   "BZHGNOCRRTCM"),
    (("I","II","III"),      "MCK", None,             "HELLOWORLD",     "DLTBBQVPQV"),
    (("III","I","II"),      "AAA", None,             "HELLOWORLD",     "KZHDFQYHXT"),
    (("I","II","III"),      "AAA", ["AB","CD","EF"], "HELLOWORLD",     "IKACBBMTBF"),
]

def verify():
    """Run all test vectors. Print results and return True if all pass."""
    all_ok = True
    for i, (rotors, key, plugs, plain, expected) in enumerate(TEST_VECTORS, 1):
        # Encrypt
        e = Enigma(rotors, key, plugs)
        cipher = e.encrypt(plain)
        ok = cipher == expected
        # Verify decryption symmetry
        d = Enigma(rotors, key, plugs)
        back = d.decrypt(cipher)
        plain_clean = "".join(c for c in plain.upper() if c in ALPHABET)
        sym_ok = back == plain_clean

        status = "PASS" if (ok and sym_ok) else "FAIL"
        print(f"  Test {i}: {plain:20s} -> {cipher:15s} [{status}]")
        if not ok:
            print(f"          Expected {expected}, got {cipher}")
        if not sym_ok:
            print(f"          Symmetry failed: decrypt gave {back}")
        all_ok &= ok and sym_ok

    print(f"\n  {'ALL 6 TESTS PASSED' if all_ok else 'SOME TESTS FAILED'}")
    return all_ok


if __name__ == "__main__":
    print("Enigma Machine — Rosetta Code Reference Implementation")
    print("=" * 55)
    verify()
