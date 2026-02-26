// Enigma cipher in UnrealScript (Unreal Engine 3)
class EnigmaCipher extends Object;

var array<int> RotorFwd1, RotorFwd2, RotorFwd3;
var array<int> RotorBwd1, RotorBwd2, RotorBwd3;
var array<int> Reflector;
var int Notches[3];
var int Pos[3];

function Init()
{
    local int i;
    local int rf1[26], rf2[26], rf3[26];
    rf1[0]=4; rf1[1]=10; rf1[2]=12; rf1[3]=5; rf1[4]=11; rf1[5]=6;
    rf1[6]=3; rf1[7]=16; rf1[8]=21; rf1[9]=25; rf1[10]=13; rf1[11]=19;
    rf1[12]=14; rf1[13]=22; rf1[14]=24; rf1[15]=7; rf1[16]=23; rf1[17]=20;
    rf1[18]=18; rf1[19]=15; rf1[20]=0; rf1[21]=8; rf1[22]=1; rf1[23]=17;
    rf1[24]=2; rf1[25]=9;
    for (i = 0; i < 26; i++) RotorFwd1.AddItem(rf1[i]);
    Reflector.Length = 26;
    Reflector[0]=24; Reflector[1]=17; Reflector[2]=20; Reflector[3]=7;
    Reflector[4]=16; Reflector[5]=18; Reflector[6]=11; Reflector[7]=3;
    Reflector[8]=15; Reflector[9]=23; Reflector[10]=13; Reflector[11]=6;
    Reflector[12]=14; Reflector[13]=10; Reflector[14]=12; Reflector[15]=8;
    Reflector[16]=4; Reflector[17]=1; Reflector[18]=5; Reflector[19]=25;
    Reflector[20]=2; Reflector[21]=22; Reflector[22]=21; Reflector[23]=9;
    Reflector[24]=0; Reflector[25]=19;
    Notches[0] = 16; Notches[1] = 4; Notches[2] = 21;
}

function int Mod26(int n)
{
    local int m;
    m = n % 26;
    if (m < 0) m += 26;
    return m;
}

function int RotorPass(array<int> Wiring, int C, int P)
{
    return Mod26(Wiring[Mod26(C + P)] - P);
}

function string Encrypt(string Text)
{
    local int i, c;
    local string Result;
    local bool mid;
    Pos[0] = 0; Pos[1] = 0; Pos[2] = 0;
    Text = Caps(Text);
    Result = "";
    for (i = 0; i < Len(Text); i++)
    {
        c = Asc(Mid(Text, i, 1)) - 65;
        if (c < 0 || c > 25) continue;
        mid = Pos[1] == Notches[1];
        if (Pos[2] == Notches[2]) Pos[2] = Mod26(Pos[2] + 1);
        if (mid || Pos[2] == Notches[2]) Pos[1] = Mod26(Pos[1] + 1);
        Pos[2] = Mod26(Pos[2] + 1);
        c = RotorPass(RotorFwd3, c, Pos[2]);
        c = RotorPass(RotorFwd2, c, Pos[1]);
        c = RotorPass(RotorFwd1, c, Pos[0]);
        c = Reflector[c];
        c = RotorPass(RotorBwd1, c, Pos[0]);
        c = RotorPass(RotorBwd2, c, Pos[1]);
        c = RotorPass(RotorBwd3, c, Pos[2]);
        Result $= Chr(c + 65);
    }
    return Result;
}

defaultproperties { }
