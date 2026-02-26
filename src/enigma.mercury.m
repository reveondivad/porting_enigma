% Enigma Machine - Mercury Implementation
% Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
% PeopleTec Inc. - Guinness World Record Attempt 2026
:- module enigma.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, char, string, list, array.

:- func fwd(int) = string.
fwd(0) = "EKMFLGDQVZNTOWYHXUSPAIBRCJ".
fwd(1) = "AJDKSIRUXBLHWTMCQGZNPYFVOE".
fwd(2) = "BDFHJLCPRTXVZNYEIWGAKMUSQO".
fwd(_) = "EKMFLGDQVZNTOWYHXUSPAIBRCJ".

:- func bwd(int) = string.
bwd(0) = "UWYGADFPVZBECKMTHXSLRINQOJ".
bwd(1) = "AJPCZWRLFBDKOTYUQGENHXMIVS".
bwd(2) = "TAGBPCSDQEUFVNZHYIXJWLRKOM".
bwd(_) = "UWYGADFPVZBECKMTHXSLRINQOJ".

:- func notch(int) = int.
notch(0) = 16. notch(1) = 4. notch(2) = 21. notch(_) = 16.

:- func reflector = string.
reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT".

:- func mod26(int) = int.
mod26(A) = ((A mod 26) + 26) mod 26.

:- func c2i(char) = int.
c2i(C) = char.to_int(C) - char.to_int('A').

:- func i2c(int) = char.
i2c(I) = char.det_from_int(I + char.to_int('A')).

:- type rotor ---> rotor(string, string, int, int).
% rotor(Fwd, Bwd, Notch, Offset)

:- func make_rotor(int, char) = rotor.
make_rotor(Num, Win) = rotor(fwd(Num), bwd(Num), notch(Num), c2i(Win)).

:- func fwd_pass(rotor, int) = int.
fwd_pass(rotor(Fwd, _, _, Off), Idx) = Result :-
    Contact = mod26(Idx + Off),
    string.det_index(Fwd, Contact, Ch),
    Result = mod26(c2i(Ch) - Off).

:- func bwd_pass(rotor, int) = int.
bwd_pass(rotor(_, Bwd, _, Off), Idx) = Result :-
    Contact = mod26(Idx + Off),
    string.det_index(Bwd, Contact, Ch),
    Result = mod26(c2i(Ch) - Off).

:- func step_rotor(rotor) = rotor.
step_rotor(rotor(F, B, N, O)) = rotor(F, B, N, (O + 1) mod 26).

:- func at_notch(rotor) = bool.
at_notch(rotor(_, _, N, O)) = (if O = N then yes else no).

:- type enigma_state ---> enigma(rotor, rotor, rotor, array(int)).

:- func make_enigma(list(int), string, list(string)) = enigma_state.
make_enigma(Rotors, Key, Plugboard) = Enigma :-
    list.det_index0(Rotors, 0, R0),
    list.det_index0(Rotors, 1, R1),
    list.det_index0(Rotors, 2, R2),
    string.det_index(Key, 0, K0),
    string.det_index(Key, 1, K1),
    string.det_index(Key, 2, K2),
    Left = make_rotor(R0, K0),
    Mid = make_rotor(R1, K1),
    Right = make_rotor(R2, K2),
    Plug0 = array.init(26, func(I) = I),
    Plug = list.foldl(
        (func(Pair, Arr) = Arr2 :-
            string.det_index(Pair, 0, CA),
            string.det_index(Pair, 1, CB),
            A = c2i(CA), B = c2i(CB),
            Arr1 = array.set(Arr, A, B),
            Arr2 = array.set(Arr1, B, A)
        ), Plugboard, Plug0),
    Enigma = enigma(Left, Mid, Right, Plug).

:- pred step_rotors(enigma_state::in, enigma_state::out) is det.
step_rotors(enigma(L0, M0, R0, P), enigma(L, M, R, P)) :-
    (if at_notch(M0) = yes then
        M1 = step_rotor(M0), L = step_rotor(L0)
    else if at_notch(R0) = yes then
        M1 = step_rotor(M0), L = L0
    else
        M1 = M0, L = L0
    ),
    M = M1,
    R = step_rotor(R0).

:- pred press_key(enigma_state::in, char::in, enigma_state::out, char::out) is det.
press_key(E0, C, E, Out) :-
    step_rotors(E0, E),
    E = enigma(Left, Mid, Right, Plug),
    Idx0 = c2i(C),
    Idx1 = array.lookup(Plug, Idx0),
    Idx2 = fwd_pass(Right, Idx1),
    Idx3 = fwd_pass(Mid, Idx2),
    Idx4 = fwd_pass(Left, Idx3),
    string.det_index(reflector, Idx4, RC),
    Idx5 = c2i(RC),
    Idx6 = bwd_pass(Left, Idx5),
    Idx7 = bwd_pass(Mid, Idx6),
    Idx8 = bwd_pass(Right, Idx7),
    Idx9 = array.lookup(Plug, Idx8),
    Out = i2c(Idx9).

:- pred encrypt(enigma_state::in, string::in, enigma_state::out, string::out) is det.
encrypt(E, Text, EFinal, Result) :-
    string.to_upper(Text, Upper),
    string.to_char_list(Upper, Chars),
    list.filter(Chars, char.is_alpha, AlphaChars),
    encrypt_chars(E, AlphaChars, EFinal, ResultChars),
    string.from_char_list(ResultChars, Result).

:- pred encrypt_chars(enigma_state::in, list(char)::in, enigma_state::out, list(char)::out) is det.
encrypt_chars(E, [], E, []).
encrypt_chars(E0, [C|Cs], EFinal, [Out|Outs]) :-
    press_key(E0, C, E1, Out),
    encrypt_chars(E1, Cs, EFinal, Outs).

main(!IO) :-
    io.write_string("Enigma Machine - Mercury Implementation\n", !IO),
    io.write_string("=======================================\n", !IO),
    Tests = [
        {[0,1,2], "AAA", [],              "AAAAA",        "BDZGO"},
        {[0,1,2], "AAA", [],              "HELLOWORLD",   "ILBDAAMTAZ"},
        {[0,1,2], "AAA", [],              "ATTACKATDAWN", "BZHGNOCRRTCM"},
        {[0,1,2], "MCK", [],              "HELLOWORLD",   "DLTBBQVPQV"},
        {[2,0,1], "AAA", [],              "HELLOWORLD",   "KZHDFQYHXT"},
        {[0,1,2], "AAA", ["AB","CD","EF"],"HELLOWORLD",   "IKACBBMTBF"}
    ],
    run_tests(Tests, 1, yes, AllPass, !IO),
    (if AllPass = yes then
        io.write_string("\n  ALL 6 TESTS PASSED\n", !IO)
    else
        io.write_string("\n  SOME TESTS FAILED\n", !IO)
    ).

:- pred run_tests(list({list(int), string, list(string), string, string})::in,
    int::in, bool::in, bool::out, io::di, io::uo) is det.
run_tests([], _, Pass, Pass, !IO).
run_tests([{Rotors, Key, Plugs, Plain, Expected}|Rest], N, PassIn, PassOut, !IO) :-
    E0 = make_enigma(Rotors, Key, Plugs),
    encrypt(E0, Plain, _, Cipher),
    (if Cipher = Expected then
        Status = "PASS", PassMid = PassIn
    else
        Status = "FAIL", PassMid = no
    ),
    io.format("  Test %d: %s -> %s [%s]\n", [i(N), s(Plain), s(Cipher), s(Status)], !IO),
    (if Cipher \= Expected then
        io.format("          Expected %s\n", [s(Expected)], !IO)
    else true),
    run_tests(Rest, N + 1, PassMid, PassOut, !IO).
