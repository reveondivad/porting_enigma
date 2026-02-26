// Enigma cipher in SourcePawn (Source Engine modding)
#include <sourcemod>

int g_RF1[] = {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9};
int g_RF2[] = {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4};
int g_RF3[] = {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14};
int g_RB1[] = {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9};
int g_RB2[] = {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18};
int g_RB3[] = {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12};
int g_Ref[] = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
int g_Notch[] = {16, 4, 21};
int g_Pos[3];

int Mod26(int n) { return ((n % 26) + 26) % 26; }

int RotorPass(int[] wiring, int c, int pos) {
    return Mod26(wiring[Mod26(c + pos)] - pos);
}

void Enigma(const char[] text, char[] output, int maxlen) {
    g_Pos[0] = 0; g_Pos[1] = 0; g_Pos[2] = 0;
    int len = strlen(text);
    int ri = 0;
    for (int i = 0; i < len && ri < maxlen - 1; i++) {
        int c = CharToUpper(text[i]) - 'A';
        if (c < 0 || c > 25) continue;
        bool mid = g_Pos[1] == g_Notch[1];
        if (g_Pos[2] == g_Notch[2]) g_Pos[2] = Mod26(g_Pos[2] + 1);
        if (mid || g_Pos[2] == g_Notch[2]) g_Pos[1] = Mod26(g_Pos[1] + 1);
        g_Pos[2] = Mod26(g_Pos[2] + 1);
        c = RotorPass(g_RF3, c, g_Pos[2]);
        c = RotorPass(g_RF2, c, g_Pos[1]);
        c = RotorPass(g_RF1, c, g_Pos[0]);
        c = g_Ref[c];
        c = RotorPass(g_RB1, c, g_Pos[0]);
        c = RotorPass(g_RB2, c, g_Pos[1]);
        c = RotorPass(g_RB3, c, g_Pos[2]);
        output[ri++] = c + 'A';
    }
    output[ri] = '\0';
}

public void OnPluginStart() {
    char result[256];
    Enigma("HELLOWORLD", result, sizeof(result));
    PrintToServer("Enigma: %s", result);
}
