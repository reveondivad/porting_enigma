%% Enigma cipher in Logtalk (OOP extension of Prolog)
:- object(enigma).
    :- public(encrypt/3).
    :- public(mod26/2).

    rotor_fwd(0, [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]).
    rotor_fwd(1, [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]).
    rotor_fwd(2, [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]).
    rotor_bwd(0, [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]).
    rotor_bwd(1, [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]).
    rotor_bwd(2, [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]).
    reflector([24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]).
    notch(0, 16). notch(1, 4). notch(2, 21).

    mod26(N, R) :- R is ((N mod 26) + 26) mod 26.

    nth0([], _, 0).
    nth0([H|_], 0, H) :- !.
    nth0([_|T], N, E) :- N1 is N - 1, nth0(T, N1, E).

    rotor_pass(Id, Dir, C, Pos, Out) :-
        (Dir = fwd -> rotor_fwd(Id, W) ; rotor_bwd(Id, W)),
        mod26(C + Pos, Idx), nth0(W, Idx, Val),
        mod26(Val - Pos, Out).

    encrypt(C, Pos, Out) :-
        Pos = [P0, P1, P2],
        rotor_pass(2, fwd, C, P2, C1), rotor_pass(1, fwd, C1, P1, C2),
        rotor_pass(0, fwd, C2, P0, C3), reflector(Ref), nth0(Ref, C3, CR),
        rotor_pass(0, bwd, CR, P0, C4), rotor_pass(1, bwd, C4, P1, C5),
        rotor_pass(2, bwd, C5, P2, Out).
:- end_object.
