# Enigma cipher in AMPL (A Mathematical Programming Language)
param N := 26;
set ALPHA := 0..N-1;

param rotor_fwd{ALPHA, 1..3};
param rotor_bwd{ALPHA, 1..3};
param reflector{ALPHA};

data;
param rotor_fwd :  1  2  3 :=
0   4  0  1    1  10  9  3    2  12  3  5    3   5 10  7
4  11 18  9    5   6  8 11    6   3 17  2    7  16 20 15
8  21 23 17    9  25  1 19   10  13 11 23   11  19  7 21
12 14 22 25   13  22 19 13   14  24 12 24   15   7  2  4
16 23 16  8   17  20  6 22   18  18 25  6   19  15 13  0
20  0 15 10   21   8 24 12   22   1  5 20   23  17 21 18
24  2 14 16   25   9  4 14;

param reflector :=
0 24  1 17  2 20  3  7  4 16  5 18  6 11  7  3
8 15  9 23 10 13 11  6 12 14 13 10 14 12 15  8
16  4 17  1 18  5 19 25 20  2 21 22 22 21 23  9
24  0 25 19;

printf "Enigma AMPL model loaded with %d rotor positions\n", N;
