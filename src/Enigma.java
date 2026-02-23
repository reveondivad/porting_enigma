// Enigma Machine — Rosetta Code Reference (Java)
// Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
// PeopleTec Inc. — Guinness World Record Attempt 2026

public class Enigma {
    static final String ALPHA = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    static final String[] FWD = {"EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO"};
    static final String[] BWD = {"UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM"};
    static final char[] NOTCH = {'Q','E','V'};
    static final String REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

    int[] offset = new int[3], id = new int[3];
    int[] plug = new int[26];

    Enigma(int[] rotors, String key, String[] plugboard) {
        for (int i = 0; i < 3; i++) { id[i] = rotors[i]; offset[i] = ALPHA.indexOf(key.charAt(i)); }
        for (int i = 0; i < 26; i++) plug[i] = i;
        if (plugboard != null) for (String p : plugboard) {
            int a = ALPHA.indexOf(p.charAt(0)), b = ALPHA.indexOf(p.charAt(1));
            plug[a] = b; plug[b] = a;
        }
    }

    int fwd(int r, int i) {
        int c = (i + offset[r]) % 26;
        return (ALPHA.indexOf(FWD[id[r]].charAt(c)) - offset[r] + 26) % 26;
    }
    int bwd(int r, int i) {
        int c = (i + offset[r]) % 26;
        return (ALPHA.indexOf(BWD[id[r]].charAt(c)) - offset[r] + 26) % 26;
    }

    void step() {
        if (ALPHA.charAt(offset[1]) == NOTCH[id[1]]) {
            offset[1] = (offset[1]+1)%26; offset[0] = (offset[0]+1)%26;
        } else if (ALPHA.charAt(offset[2]) == NOTCH[id[2]]) {
            offset[1] = (offset[1]+1)%26;
        }
        offset[2] = (offset[2]+1)%26;
    }

    char pressKey(char ch) {
        step();
        int i = ALPHA.indexOf(ch);
        i = plug[i];
        i = fwd(2,i); i = fwd(1,i); i = fwd(0,i);
        i = ALPHA.indexOf(REFLECTOR.charAt(i));
        i = bwd(0,i); i = bwd(1,i); i = bwd(2,i);
        i = plug[i];
        return ALPHA.charAt(i);
    }

    String encrypt(String text) {
        StringBuilder sb = new StringBuilder();
        for (char c : text.toUpperCase().toCharArray())
            if (c >= 'A' && c <= 'Z') sb.append(pressKey(c));
        return sb.toString();
    }

    public static void main(String[] args) {
        Object[][] tests = {
            {new int[]{0,1,2},"AAA",null,"AAAAA","BDZGO"},
            {new int[]{0,1,2},"AAA",null,"HELLOWORLD","ILBDAAMTAZ"},
            {new int[]{0,1,2},"AAA",null,"ATTACKATDAWN","BZHGNOCRRTCM"},
            {new int[]{0,1,2},"MCK",null,"HELLOWORLD","DLTBBQVPQV"},
            {new int[]{2,0,1},"AAA",null,"HELLOWORLD","KZHDFQYHXT"},
            {new int[]{0,1,2},"AAA",new String[]{"AB","CD","EF"},"HELLOWORLD","IKACBBMTBF"},
        };
        boolean allOk = true;
        for (int t = 0; t < tests.length; t++) {
            Enigma e = new Enigma((int[])tests[t][0],(String)tests[t][1],(String[])tests[t][2]);
            String ct = e.encrypt((String)tests[t][3]);
            boolean ok = ct.equals(tests[t][4]);
            System.out.printf("Test %d: %-20s -> %-15s [%s]%n", t+1, tests[t][3], ct, ok?"PASS":"FAIL");
            if (!ok) allOk = false;
        }
        System.out.println(allOk ? "\nALL 6 TESTS PASSED" : "\nSOME TESTS FAILED");
    }
}
