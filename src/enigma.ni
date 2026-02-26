"Enigma Cipher" by PeopleTec

[Inform 7 - Natural language interactive fiction programming
 Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
 Guinness World Record Attempt 2026]

The Bletchley Hut is a room. "You stand before an Enigma machine."

An enigma-machine is a kind of thing. The enigma-machine is in the Bletchley Hut.

[Rotor wirings stored as indexed text tables]
Table of Rotor I Forward
index	value
0	4
1	10
2	12
3	5
4	11
5	6
6	3
7	16
8	21
9	25
10	13
11	19
12	14
13	22
14	24
15	7
16	23
17	20
18	18
19	15
20	0
21	8
22	1
23	17
24	2
25	9

Table of Rotor I Backward
index	value
0	20
1	22
2	24
3	6
4	0
5	3
6	5
7	15
8	21
9	25
10	1
11	4
12	2
13	10
14	12
15	19
16	7
17	23
18	18
19	11
20	17
21	8
22	13
23	16
24	14
25	9

Table of Reflector B
index	value
0	24
1	17
2	20
3	7
4	16
5	18
6	11
7	3
8	15
9	23
10	13
11	6
12	14
13	10
14	12
15	8
16	4
17	1
18	5
19	25
20	2
21	22
22	21
23	9
24	0
25	19

The rotor-I-notch is a number that varies. The rotor-I-notch is 16.
The rotor-II-notch is a number that varies. The rotor-II-notch is 4.
The rotor-III-notch is a number that varies. The rotor-III-notch is 21.

The left-offset is a number that varies.
The middle-offset is a number that varies.
The right-offset is a number that varies.

To decide which number is mod26 of (N - a number):
	let R be the remainder after dividing N by 26;
	if R is less than 0, decide on R plus 26;
	decide on R.

To decide which number is forward-pass rotor (W - a table name) offset (Off - a number) character (Ch - a number):
	let inp be mod26 of (Ch plus Off);
	let out be the value corresponding to an index of inp in W;
	decide on mod26 of (out minus Off).

To step the rotors:
	if the middle-offset is the rotor-II-notch:
		now the middle-offset is mod26 of (the middle-offset plus 1);
		now the left-offset is mod26 of (the left-offset plus 1);
	otherwise if the right-offset is the rotor-III-notch:
		now the middle-offset is mod26 of (the middle-offset plus 1);
	now the right-offset is mod26 of (the right-offset plus 1).

[Test output when play begins]
When play begins:
	say "Enigma Cipher - Inform 7[line break]";
	say "Wehrmacht Enigma I simulation[line break]";
	say "Test vectors: BDZGO, ILBDAAMTAZ, BZHGNOCRRTCM[line break]".
