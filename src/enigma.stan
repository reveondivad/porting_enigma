// Enigma Cipher - Stan (Probabilistic programming language)
// Wehrmacht Enigma I configuration and test vectors
// PeopleTec Inc. - Guinness World Record Attempt 2026
// Stan is primarily for Bayesian inference; this encodes Enigma as transformed data

functions {
  int mod26(int n) {
    int m = n - 26 * (n / 26);
    if (m < 0) m = m + 26;
    return m;
  }

  int get_fwd(array[] int fI, array[] int fII, array[] int fIII, int r, int i) {
    if (r == 0) return fI[i + 1];  // Stan is 1-indexed
    else if (r == 1) return fII[i + 1];
    else return fIII[i + 1];
  }

  int get_bwd(array[] int bI, array[] int bII, array[] int bIII, int r, int i) {
    if (r == 0) return bI[i + 1];
    else if (r == 1) return bII[i + 1];
    else return bIII[i + 1];
  }

  int pass_fwd(array[] int fI, array[] int fII, array[] int fIII,
               int rotor, int offset, int ch) {
    int inp = mod26(ch + offset);
    int out = get_fwd(fI, fII, fIII, rotor, inp);
    return mod26(out - offset);
  }

  int pass_bwd(array[] int bI, array[] int bII, array[] int bIII,
               int rotor, int offset, int ch) {
    int inp = mod26(ch + offset);
    int out = get_bwd(bI, bII, bIII, rotor, inp);
    return mod26(out - offset);
  }
}

transformed data {
  array[26] int fwdI  = {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9};
  array[26] int fwdII = {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4};
  array[26] int fwdIII= {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14};
  array[26] int bwdI  = {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9};
  array[26] int bwdII = {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18};
  array[26] int bwdIII= {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12};
  array[26] int ref   = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
  // Test AAAAA: Expected output indices = {1,3,25,6,14} = BDZGO
}

parameters { real dummy; }
model { dummy ~ normal(0, 1); }
