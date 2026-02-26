%% Enigma Machine - Prolog Implementation (SWI-Prolog)
%% Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
%% PeopleTec Inc. - Guinness World Record Attempt 2026

:- use_module(library(lists)).

fwd(1, "EKMFLGDQVZNTOWYHXUSPAIBRCJ").
fwd(2, "AJDKSIRUXBLHWTMCQGZNPYFVOE").
fwd(3, "BDFHJLCPRTXVZNYEIWGAKMUSQO").
bwd(1, "UWYGADFPVZBECKMTHXSLRINQOJ").
bwd(2, "AJPCZWRLFBDKOTYUQGENHXMIVS").
bwd(3, "TAGBPCSDQEUFVNZHYIXJWLRKOM").
notch(1, 16). notch(2, 4). notch(3, 21).
refl("YRUHQSLDPXNGOKMIEBFZCWVJAT").

mod26(A, R) :- R is ((A mod 26) + 26) mod 26.
c2i(C, I) :- I is C - 0'A.
i2c(I, C) :- C is I + 0'A.

nth0_char(Str, Idx, Char) :-
    string_code(Idx1, Str, Char), Idx1 is Idx + 1, !.
nth0_char(Str, Idx, Char) :-
    atom_codes(Str, Codes),
    nth0(Idx, Codes, Char).

rotor_pass(Wiring, Offset, Idx, Out) :-
    mod26(Idx + Offset, Contact),
    nth0_char(Wiring, Contact, Ch),
    c2i(Ch, ChIdx),
    mod26(ChIdx - Offset, Out).

%% State: state(LeftOff, MidOff, RightOff, LeftFwd, LeftBwd, LeftNotch,
%%              MidFwd, MidBwd, MidNotch, RightFwd, RightBwd, RightNotch, Plug)

make_plug([], Plug, Plug).
make_plug([Pair|Rest], PlugIn, PlugOut) :-
    atom_codes(Pair, [A0, B0]),
    A is A0 - 0'A, B is B0 - 0'A,
    replace_nth(A, B, PlugIn, Plug1),
    replace_nth(B, A, Plug1, Plug2),
    make_plug(Rest, Plug2, PlugOut).

replace_nth(0, Val, [_|T], [Val|T]) :- !.
replace_nth(N, Val, [H|T], [H|T2]) :- N > 0, N1 is N - 1, replace_nth(N1, Val, T, T2).

identity_plug(Plug) :- numlist(0, 25, Plug).

init_state(R1, R2, R3, Key, Plugboard, State) :-
    atom_codes(Key, [K1, K2, K3]),
    fwd(R1, LF), bwd(R1, LB), notch(R1, LN), c2i(K1, LO),
    fwd(R2, MF), bwd(R2, MB), notch(R2, MN), c2i(K2, MO),
    fwd(R3, RF), bwd(R3, RB), notch(R3, RN), c2i(K3, RO),
    identity_plug(P0),
    make_plug(Plugboard, P0, Plug),
    State = state(LO, MO, RO, LF, LB, LN, MF, MB, MN, RF, RB, RN, Plug).

step_rotors(state(LO, MO, RO, LF, LB, LN, MF, MB, MN, RF, RB, RN, Plug),
            state(LO2, MO2, RO2, LF, LB, LN, MF, MB, MN, RF, RB, RN, Plug)) :-
    (MO =:= MN ->
        MO1 is (MO + 1) mod 26, LO2 is (LO + 1) mod 26
    ; RO =:= RN ->
        MO1 is (MO + 1) mod 26, LO2 = LO
    ;
        MO1 = MO, LO2 = LO
    ),
    MO2 = MO1,
    RO2 is (RO + 1) mod 26.

plug_lookup(Plug, Idx, Out) :- nth0(Idx, Plug, Out).

press_key(StateIn, Char, StateOut, OutChar) :-
    step_rotors(StateIn, S),
    S = state(LO, MO, RO, LF, LB, _, MF, MB, _, RF, RB, _, Plug),
    c2i(Char, Idx0),
    plug_lookup(Plug, Idx0, Idx1),
    rotor_pass(RF, RO, Idx1, Idx2),
    rotor_pass(MF, MO, Idx2, Idx3),
    rotor_pass(LF, LO, Idx3, Idx4),
    refl(Refl), nth0_char(Refl, Idx4, RC), c2i(RC, Idx5),
    rotor_pass(LB, LO, Idx5, Idx6),
    rotor_pass(MB, MO, Idx6, Idx7),
    rotor_pass(RB, RO, Idx7, Idx8),
    plug_lookup(Plug, Idx8, Idx9),
    i2c(Idx9, OutChar),
    StateOut = S.

encrypt(State, [], State, []).
encrypt(StateIn, [C|Cs], StateOut, [O|Os]) :-
    C >= 0'A, C =< 0'Z, !,
    press_key(StateIn, C, S2, O),
    encrypt(S2, Cs, StateOut, Os).
encrypt(StateIn, [_|Cs], StateOut, Os) :-
    encrypt(StateIn, Cs, StateOut, Os).

encrypt_string(State, Text, Cipher) :-
    upcase_atom(Text, Upper),
    atom_codes(Upper, Codes),
    encrypt(State, Codes, _, OutCodes),
    atom_codes(Cipher, OutCodes).

run_test(N, R1, R2, R3, Key, Plugs, Plain, Expected) :-
    init_state(R1, R2, R3, Key, Plugs, State),
    encrypt_string(State, Plain, Cipher),
    (Cipher == Expected ->
        format("  Test ~w: ~w -> ~w [PASS]~n", [N, Plain, Cipher])
    ;
        format("  Test ~w: ~w -> ~w [FAIL] expected ~w~n", [N, Plain, Cipher, Expected]),
        nb_setval(all_pass, false)
    ).

main :-
    write('Enigma Machine - Prolog Implementation'), nl,
    write('======================================='), nl,
    nb_setval(all_pass, true),
    run_test(1, 1,2,3, 'AAA', [], 'AAAAA', 'BDZGO'),
    run_test(2, 1,2,3, 'AAA', [], 'HELLOWORLD', 'ILBDAAMTAZ'),
    run_test(3, 1,2,3, 'AAA', [], 'ATTACKATDAWN', 'BZHGNOCRRTCM'),
    run_test(4, 1,2,3, 'MCK', [], 'HELLOWORLD', 'DLTBBQVPQV'),
    run_test(5, 3,1,2, 'AAA', [], 'HELLOWORLD', 'KZHDFQYHXT'),
    run_test(6, 1,2,3, 'AAA', ['AB','CD','EF'], 'HELLOWORLD', 'IKACBBMTBF'),
    nb_getval(all_pass, Result),
    (Result == true ->
        write('  ALL 6 TESTS PASSED'), nl
    ;
        write('  SOME TESTS FAILED'), nl
    ).

:- main.