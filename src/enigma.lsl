// Enigma cipher in LSL (Linden Scripting Language - Second Life)
list ROTOR_FWD_1 = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
list ROTOR_FWD_2 = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
list ROTOR_FWD_3 = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];
list ROTOR_BWD_1 = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
list ROTOR_BWD_2 = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
list ROTOR_BWD_3 = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12];
list REFLECTOR = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
list NOTCHES = [16, 4, 21];

integer mod26(integer n) { return ((n % 26) + 26) % 26; }

integer rotorPass(list wiring, integer c, integer p) {
    return mod26(llList2Integer(wiring, mod26(c + p)) - p);
}

string enigma(string text) {
    list pos = [0, 0, 0];
    string result = "";
    integer i;
    text = llToUpper(text);
    for (i = 0; i < llStringLength(text); i++) {
        string ch = llGetSubString(text, i, i);
        integer c = (integer)llOrd(ch, 0) - 65;
        if (c >= 0 && c < 26) {
            integer mid = llList2Integer(pos, 1) == llList2Integer(NOTCHES, 1);
            if (llList2Integer(pos, 2) == llList2Integer(NOTCHES, 2))
                pos = llListReplaceList(pos, [mod26(llList2Integer(pos, 2) + 1)], 2, 2);
            if (mid || llList2Integer(pos, 2) == llList2Integer(NOTCHES, 2))
                pos = llListReplaceList(pos, [mod26(llList2Integer(pos, 1) + 1)], 1, 1);
            pos = llListReplaceList(pos, [mod26(llList2Integer(pos, 2) + 1)], 2, 2);
            c = rotorPass(ROTOR_FWD_3, c, llList2Integer(pos, 2));
            c = rotorPass(ROTOR_FWD_2, c, llList2Integer(pos, 1));
            c = rotorPass(ROTOR_FWD_1, c, llList2Integer(pos, 0));
            c = llList2Integer(REFLECTOR, c);
            c = rotorPass(ROTOR_BWD_1, c, llList2Integer(pos, 0));
            c = rotorPass(ROTOR_BWD_2, c, llList2Integer(pos, 1));
            c = rotorPass(ROTOR_BWD_3, c, llList2Integer(pos, 2));
            result += llChar(c + 65);
        }
    }
    return result;
}

default { state_entry() { llSay(0, enigma("HELLOWORLD")); } }
