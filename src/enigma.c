/* Enigma Machine — Rosetta Code Reference (C)
 * Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
 * PeopleTec Inc. — Guinness World Record Attempt 2026
 *
 * Test vectors:
 *   I II III / AAA / no plugboard / AAAAA        -> BDZGO
 *   I II III / AAA / no plugboard / HELLOWORLD   -> ILBDAAMTAZ
 *   I II III / AAA / no plugboard / ATTACKATDAWN -> BZHGNOCRRTCM
 */
#include <stdio.h>
#include <string.h>
#include <ctype.h>

static const char *ALPHA = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

static const char *FWD[] = {
    "EKMFLGDQVZNTOWYHXUSPAIBRCJ",   /* I   */
    "AJDKSIRUXBLHWTMCQGZNPYFVOE",   /* II  */
    "BDFHJLCPRTXVZNYEIWGAKMUSQO"    /* III */
};
static const char *BWD[] = {
    "UWYGADFPVZBECKMTHXSLRINQOJ",
    "AJPCZWRLFBDKOTYUQGENHXMIVS",
    "TAGBPCSDQEUFVNZHYIXJWLRKOM"
};
static const char NOTCH[] = { 'Q', 'E', 'V' };  /* I, II, III */
static const char *REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT";

static int idx(char c) { return c - 'A'; }

typedef struct { int fwd_id; int offset; char notch; } Rotor;

void rotor_init(Rotor *r, int id, char window) {
    r->fwd_id = id; r->offset = idx(window); r->notch = NOTCH[id];
}
int rotor_at_notch(Rotor *r) { return ALPHA[r->offset] == r->notch; }
void rotor_step(Rotor *r) { r->offset = (r->offset + 1) % 26; }

int rotor_fwd(Rotor *r, int i) {
    int contact = (i + r->offset) % 26;
    int out = idx(FWD[r->fwd_id][contact]);
    return (out - r->offset + 26) % 26;
}
int rotor_bwd(Rotor *r, int i) {
    int contact = (i + r->offset) % 26;
    int out = idx(BWD[r->fwd_id][contact]);
    return (out - r->offset + 26) % 26;
}

typedef struct {
    Rotor left, middle, right;
    int plug[26];
} Enigma;

void enigma_init(Enigma *e, int r0, int r1, int r2,
                 const char *key, const char *plugs[], int nplugs) {
    rotor_init(&e->left,   r0, key[0]);
    rotor_init(&e->middle, r1, key[1]);
    rotor_init(&e->right,  r2, key[2]);
    for (int i = 0; i < 26; i++) e->plug[i] = i;
    for (int i = 0; i < nplugs; i++) {
        int a = idx(plugs[i][0]), b = idx(plugs[i][1]);
        e->plug[a] = b; e->plug[b] = a;
    }
}

void enigma_step(Enigma *e) {
    if (rotor_at_notch(&e->middle)) {
        rotor_step(&e->middle);
        rotor_step(&e->left);
    } else if (rotor_at_notch(&e->right)) {
        rotor_step(&e->middle);
    }
    rotor_step(&e->right);
}

char enigma_press(Enigma *e, char c) {
    enigma_step(e);
    int i = idx(c);
    i = e->plug[i];
    i = rotor_fwd(&e->right, i);
    i = rotor_fwd(&e->middle, i);
    i = rotor_fwd(&e->left, i);
    i = idx(REFLECTOR[i]);
    i = rotor_bwd(&e->left, i);
    i = rotor_bwd(&e->middle, i);
    i = rotor_bwd(&e->right, i);
    i = e->plug[i];
    return ALPHA[i];
}

void enigma_encrypt(Enigma *e, const char *in, char *out) {
    int j = 0;
    for (int i = 0; in[i]; i++) {
        char c = toupper(in[i]);
        if (c >= 'A' && c <= 'Z') out[j++] = enigma_press(e, c);
    }
    out[j] = '\0';
}

int main(void) {
    typedef struct { int r[3]; const char *key; const char **plugs; int np;
                     const char *plain; const char *expected; } TV;

    const char *p6[] = {"AB","CD","EF"};
    TV tests[] = {
        {{0,1,2}, "AAA", NULL,0, "AAAAA",        "BDZGO"},
        {{0,1,2}, "AAA", NULL,0, "HELLOWORLD",   "ILBDAAMTAZ"},
        {{0,1,2}, "AAA", NULL,0, "ATTACKATDAWN", "BZHGNOCRRTCM"},
        {{0,1,2}, "MCK", NULL,0, "HELLOWORLD",   "DLTBBQVPQV"},
        {{2,0,1}, "AAA", NULL,0, "HELLOWORLD",   "KZHDFQYHXT"},
        {{0,1,2}, "AAA", p6, 3,  "HELLOWORLD",   "IKACBBMTBF"},
    };
    int all_ok = 1;
    for (int t = 0; t < 6; t++) {
        Enigma e;
        enigma_init(&e, tests[t].r[0], tests[t].r[1], tests[t].r[2],
                    tests[t].key, tests[t].plugs, tests[t].np);
        char buf[256];
        enigma_encrypt(&e, tests[t].plain, buf);
        int ok = strcmp(buf, tests[t].expected) == 0;
        printf("Test %d: %-20s -> %-15s [%s]\n", t+1, tests[t].plain, buf,
               ok ? "PASS" : "FAIL");
        if (!ok) all_ok = 0;
    }
    printf("\n%s\n", all_ok ? "ALL 6 TESTS PASSED" : "SOME TESTS FAILED");
    return !all_ok;
}
