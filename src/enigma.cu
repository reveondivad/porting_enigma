// Enigma Cipher - CUDA Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026
// Parallel encryption of multiple messages on GPU

#include <stdio.h>
#include <string.h>

__constant__ int d_fwd[3][26] = {
    {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9},
    {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4},
    {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14}
};
__constant__ int d_bwd[3][26] = {
    {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9},
    {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18},
    {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12}
};
__constant__ int d_ref[26] = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
__constant__ int d_notch[3] = {16, 4, 21};

__device__ int mod26(int n) { return ((n % 26) + 26) % 26; }

__device__ int fwd_pass(int rotor, int offset, int ch) {
    int inp = mod26(ch + offset);
    int out = d_fwd[rotor][inp];
    return mod26(out - offset);
}

__device__ int bwd_pass(int rotor, int offset, int ch) {
    int inp = mod26(ch + offset);
    int out = d_bwd[rotor][inp];
    return mod26(out - offset);
}

// Each thread encrypts one complete message
__global__ void enigma_encrypt(char* messages, char* outputs, int* lengths,
                                int r0, int r1, int r2, int k0, int k1, int k2,
                                int max_len, int num_messages) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;
    if (tid >= num_messages) return;

    int pb[26];
    for (int i = 0; i < 26; i++) pb[i] = i;

    int o0 = k0, o1 = k1, o2 = k2;
    int n1 = d_notch[r1], n2 = d_notch[r2];
    int offset = tid * max_len;
    int len = lengths[tid];

    for (int i = 0; i < len; i++) {
        int ch = messages[offset + i] - 'A';
        int mid = (o1 == n1);
        int atn = (o2 == n2);
        o2 = mod26(o2 + 1);
        if (atn || mid) o1 = mod26(o1 + 1);
        if (mid) o0 = mod26(o0 + 1);

        int c = pb[ch];
        c = fwd_pass(r2, o2, c);
        c = fwd_pass(r1, o1, c);
        c = fwd_pass(r0, o0, c);
        c = d_ref[c];
        c = bwd_pass(r0, o0, c);
        c = bwd_pass(r1, o1, c);
        c = bwd_pass(r2, o2, c);
        c = pb[c];
        outputs[offset + i] = (char)(c + 'A');
    }
}

// Host-side single message encrypt for testing
void host_encrypt(int r0, int r1, int r2, int k0, int k1, int k2,
                  const char* msg, char* out) {
    int fwd[3][26] = {
        {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9},
        {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4},
        {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14}
    };
    int bwd[3][26] = {
        {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9},
        {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18},
        {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12}
    };
    int ref[26] = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
    int notch[3] = {16, 4, 21};
    int pb[26]; for (int i=0;i<26;i++) pb[i]=i;
    int o0=k0, o1=k1, o2=k2;
    int n1=notch[r1], n2=notch[r2];
    int len=strlen(msg);
    for (int i=0; i<len; i++) {
        int ch=msg[i]-'A';
        int mid=(o1==n1), atn=(o2==n2);
        o2=((o2+1)%26+26)%26;
        if(atn||mid) o1=((o1+1)%26+26)%26;
        if(mid) o0=((o0+1)%26+26)%26;
        int c=pb[ch];
        c=((fwd[r2][((c+o2)%26+26)%26]-o2)%26+26)%26;
        c=((fwd[r1][((c+o1)%26+26)%26]-o1)%26+26)%26;
        c=((fwd[r0][((c+o0)%26+26)%26]-o0)%26+26)%26;
        c=ref[c];
        c=((bwd[r0][((c+o0)%26+26)%26]-o0)%26+26)%26;
        c=((bwd[r1][((c+o1)%26+26)%26]-o1)%26+26)%26;
        c=((bwd[r2][((c+o2)%26+26)%26]-o2)%26+26)%26;
        c=pb[c];
        out[i]=(char)(c+'A');
    }
    out[len]=0;
}

int main() {
    printf("Enigma Cipher - CUDA Implementation\n");
    char buf[64];
    struct { int r0,r1,r2,k0,k1,k2; const char *msg, *exp; } tests[] = {
        {0,1,2,0,0,0,"AAAAA","BDZGO"},
        {0,1,2,0,0,0,"HELLOWORLD","ILBDAAMTAZ"},
        {0,1,2,0,0,0,"ATTACKATDAWN","BZHGNOCRRTCM"},
        {0,1,2,12,2,10,"HELLOWORLD","DLTBBQVPQV"},
        {2,0,1,0,0,0,"HELLOWORLD","KZHDFQYHXT"},
    };
    for (int t=0; t<5; t++) {
        host_encrypt(tests[t].r0,tests[t].r1,tests[t].r2,
                     tests[t].k0,tests[t].k1,tests[t].k2,tests[t].msg,buf);
        printf("Test %d: %s -> %s %s\n", t+1, tests[t].msg, buf,
               strcmp(buf,tests[t].exp)==0 ? "[PASS]" : "[FAIL]");
    }
    return 0;
}
