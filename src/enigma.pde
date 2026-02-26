// Enigma Machine - Processing Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026

String[] FWD = {"EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"};
String[] BWD = {"UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"};
String REF = "YRUHQSLDPXNGOKMIEBFZCWVJAT";
int[] NOTCH = {16, 4, 21};

int[] rotors = new int[3];
int[] offsets = new int[3];
int[] notches = new int[3];
int[] plugboard = new int[26];

int mod26(int n) { return ((n % 26) + 26) % 26; }

void initEnigma(int r1, int r2, int r3, int k1, int k2, int k3) {
  rotors[0] = r1-1; rotors[1] = r2-1; rotors[2] = r3-1;
  offsets[0] = k1; offsets[1] = k2; offsets[2] = k3;
  for (int i = 0; i < 3; i++) notches[i] = NOTCH[rotors[i]];
  for (int i = 0; i < 26; i++) plugboard[i] = i;
}

void stepRotors() {
  if (offsets[1] == notches[1]) {
    offsets[1] = mod26(offsets[1] + 1);
    offsets[0] = mod26(offsets[0] + 1);
  } else if (offsets[2] == notches[2]) {
    offsets[1] = mod26(offsets[1] + 1);
  }
  offsets[2] = mod26(offsets[2] + 1);
}

int fwdPass(int rotor, int idx) {
  int contact = mod26(idx + offsets[rotor]);
  int out = FWD[rotors[rotor]].charAt(contact) - 'A';
  return mod26(out - offsets[rotor]);
}

int bwdPass(int rotor, int idx) {
  int contact = mod26(idx + offsets[rotor]);
  int out = BWD[rotors[rotor]].charAt(contact) - 'A';
  return mod26(out - offsets[rotor]);
}

char pressKey(char c) {
  stepRotors();
  int idx = plugboard[c - 'A'];
  idx = fwdPass(2, idx); idx = fwdPass(1, idx); idx = fwdPass(0, idx);
  idx = REF.charAt(idx) - 'A';
  idx = bwdPass(0, idx); idx = bwdPass(1, idx); idx = bwdPass(2, idx);
  idx = plugboard[idx];
  return char('A' + idx);
}

String encrypt(String text) {
  text = text.toUpperCase();
  StringBuilder result = new StringBuilder();
  for (int i = 0; i < text.length(); i++) {
    char c = text.charAt(i);
    if (c >= 'A' && c <= 'Z') result.append(pressKey(c));
  }
  return result.toString();
}

void setup() {
  size(400, 300);
  background(0);
  fill(0, 255, 0);
  textSize(14);
  int y = 30;
  text("Enigma Machine - Processing", 20, y); y += 25;

  String[][] tests = {
    {"1","2","3","0","0","0","AAAAA","BDZGO"},
    {"1","2","3","0","0","0","HELLOWORLD","ILBDAAMTAZ"},
    {"1","2","3","0","0","0","ATTACKATDAWN","BZHGNOCRRTCM"},
    {"1","2","3","12","2","10","HELLOWORLD","DLTBBQVPQV"},
    {"3","1","2","0","0","0","HELLOWORLD","KZHDFQYHXT"},
  };

  for (int t = 0; t < tests.length; t++) {
    initEnigma(int(tests[t][0]), int(tests[t][1]), int(tests[t][2]),
               int(tests[t][3]), int(tests[t][4]), int(tests[t][5]));
    String result = encrypt(tests[t][6]);
    boolean ok = result.equals(tests[t][7]);
    text("Test " + (t+1) + ": " + tests[t][6] + " -> " + result +
         (ok ? " [PASS]" : " [FAIL]"), 20, y);
    y += 20;
  }
}

void draw() {}
