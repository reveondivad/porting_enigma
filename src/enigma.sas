/* Enigma Cipher - SAS
   Statistical Analysis System
   Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
   PeopleTec Inc. - Guinness World Record Attempt 2026 */

data enigma_wirings;
    array fwdI[26]  (4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9);
    array fwdII[26] (0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4);
    array fwdIII[26](1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14);
    array bwdI[26]  (20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9);
    array bwdII[26] (0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18);
    array bwdIII[26](19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12);
    array ref[26]   (24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19);
    array notch[3]  (16 4 21);
run;

%macro mod26(n);
    %let m = %sysfunc(mod(&n, 26));
    %if &m < 0 %then %eval(&m + 26);
    %else &m;
%mend mod26;

/* Main encryption data step */
data enigma_result;
    length msg $20 result $20;
    msg = "AAAAA";
    
    array fwdI[26]  _temporary_ (4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9);
    array fwdII[26] _temporary_ (0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4);
    array fwdIII[26]_temporary_ (1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14);
    array bwdI[26]  _temporary_ (20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9);
    array bwdII[26] _temporary_ (0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18);
    array bwdIII[26]_temporary_ (19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12);
    array ref[26]   _temporary_ (24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19);
    
    o0=0; o1=0; o2=0; n1=4; n2=21;
    result = "";
    
    do i = 1 to length(msg);
        /* Step */
        if o1 = n1 then do; o1 = mod(o1+1, 26); o0 = mod(o0+1, 26); end;
        else if o2 = n2 then o1 = mod(o1+1, 26);
        o2 = mod(o2+1, 26);
        
        c = rank(substr(msg, i, 1)) - 65;
        /* Forward: III, II, I */
        inp = mod(c + o2, 26); c = mod(fwdIII[inp+1] - o2 + 26, 26);
        inp = mod(c + o1, 26); c = mod(fwdII[inp+1] - o1 + 26, 26);
        inp = mod(c + o0, 26); c = mod(fwdI[inp+1] - o0 + 26, 26);
        /* Reflector */
        c = ref[c+1];
        /* Backward: I, II, III */
        inp = mod(c + o0, 26); c = mod(bwdI[inp+1] - o0 + 26, 26);
        inp = mod(c + o1, 26); c = mod(bwdII[inp+1] - o1 + 26, 26);
        inp = mod(c + o2, 26); c = mod(bwdIII[inp+1] - o2 + 26, 26);
        result = cats(result, byte(c + 65));
    end;
    
    put "Enigma Cipher - SAS";
    put "Test: " result " expected BDZGO";
run;
