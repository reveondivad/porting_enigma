// Enigma cipher in ZenScript (Minecraft CraftTweaker)
val rotorFwd1 as int[] = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
val rotorFwd2 as int[] = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
val rotorFwd3 as int[] = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];
val rotorBwd1 as int[] = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
val rotorBwd2 as int[] = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
val rotorBwd3 as int[] = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12];
val reflector as int[] = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
val notches as int[] = [16, 4, 21];

function mod26(n as int) as int {
    var m = n % 26;
    if (m < 0) { m = m + 26; }
    return m;
}

function rotorPass(wiring as int[], c as int, pos as int) as int {
    return mod26(wiring[mod26(c + pos)] - pos);
}

function enigma(text as string) as string {
    var pos = [0, 0, 0] as int[];
    var result = "";
    for ch in text.toUpperCase() {
        var c = ch as int - 65;
        if (c >= 0 && c < 26) {
            var mid = pos[1] == notches[1];
            if (pos[2] == notches[2]) { pos[2] = mod26(pos[2] + 1); }
            if (mid || pos[2] == notches[2]) { pos[1] = mod26(pos[1] + 1); }
            pos[2] = mod26(pos[2] + 1);
            c = rotorPass(rotorFwd3, c, pos[2]);
            c = rotorPass(rotorFwd2, c, pos[1]);
            c = rotorPass(rotorFwd1, c, pos[0]);
            c = reflector[c];
            c = rotorPass(rotorBwd1, c, pos[0]);
            c = rotorPass(rotorBwd2, c, pos[1]);
            c = rotorPass(rotorBwd3, c, pos[2]);
            result = result + (c + 65) as string;
        }
    }
    return result;
}

print(enigma("HELLOWORLD"));
