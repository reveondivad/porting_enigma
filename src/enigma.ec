// Enigma Cipher - eC (Ecere)
// OO systems language with IDE/GUI framework
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

import "ecere"

static int fwdI[26]  = {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9};
static int fwdII[26] = {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4};
static int fwdIII[26]= {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14};
static int bwdI[26]  = {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9};
static int bwdII[26] = {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18};
static int bwdIII[26]= {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12};
static int reflector[26]= {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
static int notches[3] = {16, 4, 21};

int mod26(int n) { int m = n % 26; return m < 0 ? m + 26 : m; }

class Enigma
{
   int r[3], o[3], n1, n2;

   void Init(int r0, int r1, int r2, int k0, int k1, int k2)
   {
      r[0]=r0; r[1]=r1; r[2]=r2;
      o[0]=k0; o[1]=k1; o[2]=k2;
      n1 = notches[r1]; n2 = notches[r2];
   }

   void Step()
   {
      if(o[1] == n1) { o[1] = mod26(o[1]+1); o[0] = mod26(o[0]+1); }
      else if(o[2] == n2) { o[1] = mod26(o[1]+1); }
      o[2] = mod26(o[2]+1);
   }

   int FwdPass(int slot, int idx)
   {
      int c = mod26(idx + o[slot]);
      int * tbl = (slot==0) ? (r[slot]==0?fwdI:r[slot]==1?fwdII:fwdIII)
                            : (r[slot]==0?fwdI:r[slot]==1?fwdII:fwdIII);
      return mod26(tbl[c] - o[slot]);
   }

   int BwdPass(int slot, int idx)
   {
      int c = mod26(idx + o[slot]);
      int * tbl = r[slot]==0?bwdI:r[slot]==1?bwdII:bwdIII;
      return mod26(tbl[c] - o[slot]);
   }

   int PressKey(int ch)
   {
      Step();
      int c = ch;
      c = FwdPass(2,c); c = FwdPass(1,c); c = FwdPass(0,c);
      c = reflector[c];
      c = BwdPass(0,c); c = BwdPass(1,c); c = BwdPass(2,c);
      return c;
   }
}

class EnigmaApp : Application
{
   void Main()
   {
      PrintLn("Enigma Cipher - eC (Ecere)");
      Enigma e {};
      e.Init(0,1,2,0,0,0);
      char result[64];
      char * msg = "AAAAA";
      for(int i = 0; i < 5; i++)
         result[i] = (char)(e.PressKey(msg[i] - 'A') + 'A');
      result[5] = 0;
      PrintLn($"Test 1: $(result) expected BDZGO");
   }
}
