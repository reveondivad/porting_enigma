// Enigma Machine — Rosetta Code Reference (Dart)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026

const fwd = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"];
const bwd = ["UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"];
const notch = [81, 69, 86]; // Q E V
const reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

class Enigma {
  List<int> id, off, plug;
  Enigma(this.id, String key, [List<String> plugboard = const []])
    : off = key.codeUnits.map((c) => c - 65).toList(),
      plug = List.generate(26, (i) => i) {
    for (var p in plugboard) {
      int a = p.codeUnitAt(0)-65, b = p.codeUnitAt(1)-65;
      plug[a] = b; plug[b] = a;
    }
  }

  int _fwd(int r, int i) { int c=(i+off[r])%26; return(fwd[id[r]].codeUnitAt(c)-65-off[r]+26)%26; }
  int _bwd(int r, int i) { int c=(i+off[r])%26; return(bwd[id[r]].codeUnitAt(c)-65-off[r]+26)%26; }

  void _step() {
    if (off[1]+65 == notch[id[1]]) { off[1]=(off[1]+1)%26; off[0]=(off[0]+1)%26; }
    else if (off[2]+65 == notch[id[2]]) { off[1]=(off[1]+1)%26; }
    off[2]=(off[2]+1)%26;
  }

  String pressKey(String ch) {
    _step(); int i = ch.codeUnitAt(0)-65;
    i=plug[i];
    i=_fwd(2,i); i=_fwd(1,i); i=_fwd(0,i);
    i=reflector.codeUnitAt(i)-65;
    i=_bwd(0,i); i=_bwd(1,i); i=_bwd(2,i);
    i=plug[i];
    return String.fromCharCode(i+65);
  }

  String encrypt(String text) =>
    text.toUpperCase().replaceAll(RegExp(r'[^A-Z]'),'').split('').map((c)=>pressKey(c)).join();
}

void main() {
  var tests = [
    [[0,1,2],"AAA",<String>[],"AAAAA","BDZGO"],
    [[0,1,2],"AAA",<String>[],"HELLOWORLD","ILBDAAMTAZ"],
    [[0,1,2],"AAA",<String>[],"ATTACKATDAWN","BZHGNOCRRTCM"],
    [[0,1,2],"MCK",<String>[],"HELLOWORLD","DLTBBQVPQV"],
    [[2,0,1],"AAA",<String>[],"HELLOWORLD","KZHDFQYHXT"],
    [[0,1,2],"AAA",["AB","CD","EF"],"HELLOWORLD","IKACBBMTBF"],
  ];
  bool allOk = true;
  for (int t = 0; t < tests.length; t++) {
    var r = (tests[t][0] as List).cast<int>();
    var e = Enigma(r, tests[t][1] as String, (tests[t][2] as List).cast<String>());
    var ct = e.encrypt(tests[t][3] as String);
    var ok = ct == tests[t][4];
    print("Test ${t+1}: ${(tests[t][3] as String).padRight(20)} -> ${ct.padRight(15)} [${ok?'PASS':'FAIL'}]");
    if (!ok) allOk = false;
  }
  print(allOk ? "\nALL 6 TESTS PASSED" : "\nSOME TESTS FAILED");
}
