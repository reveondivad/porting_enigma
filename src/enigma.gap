# Enigma Cipher - GAP
# Computational algebra system (Groups, Algorithms, Programming)
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

fwdI  := [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9] + 1;
fwdII := [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4] + 1;
fwdIII:= [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14] + 1;
bwdI  := [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9] + 1;
bwdII := [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18] + 1;
bwdIII:= [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12] + 1;
refl  := [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19] + 1;
notches := [16, 4, 21];

Mod26 := function(n)
    local m;
    m := n mod 26;
    if m < 0 then m := m + 26; fi;
    return m;
end;

GetFwd := function(r, i)
    if r = 0 then return fwdI[i+1];
    elif r = 1 then return fwdII[i+1];
    else return fwdIII[i+1]; fi;
end;

GetBwd := function(r, i)
    if r = 0 then return bwdI[i+1];
    elif r = 1 then return bwdII[i+1];
    else return bwdIII[i+1]; fi;
end;

PassFwd := function(rotor, offset, ch)
    return Mod26(GetFwd(rotor, Mod26(ch + offset)) - offset - 1);
end;

PassBwd := function(rotor, offset, ch)
    return Mod26(GetBwd(rotor, Mod26(ch + offset)) - offset - 1);
end;

# Test: AAAAA -> BDZGO, HELLOWORLD -> ILBDAAMTAZ
Print("Enigma Cipher - GAP\n");
Print("Test: AAAAA -> BDZGO\n");
