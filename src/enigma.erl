%% Enigma Machine - Erlang Implementation
%% Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
%% PeopleTec Inc. - Guinness World Record Attempt 2026

-module(enigma).
-export([main/0]).

fwd(1) -> "EKMFLGDQVZNTOWYHXUSPAIBRCJ";
fwd(2) -> "AJDKSIRUXBLHWTMCQGZNPYFVOE";
fwd(3) -> "BDFHJLCPRTXVZNYEIWGAKMUSQO".

bwd(1) -> "UWYGADFPVZBECKMTHXSLRINQOJ";
bwd(2) -> "AJPCZWRLFBDKOTYUQGENHXMIVS";
bwd(3) -> "TAGBPCSDQEUFVNZHYIXJWLRKOM".

notch(1) -> 16; notch(2) -> 4; notch(3) -> 21.

refl() -> "YRUHQSLDPXNGOKMIEBFZCWVJAT".

mod26(A) -> ((A rem 26) + 26) rem 26.

make_rotor(Num, Win) ->
    #{fwd => fwd(Num), bwd => bwd(Num),
      notch => notch(Num), offset => Win - $A}.

fwd_pass(#{fwd := F, offset := Off}, Idx) ->
    Contact = mod26(Idx + Off),
    Out = lists:nth(Contact + 1, F) - $A,
    mod26(Out - Off).

bwd_pass(#{bwd := B, offset := Off}, Idx) ->
    Contact = mod26(Idx + Off),
    Out = lists:nth(Contact + 1, B) - $A,
    mod26(Out - Off).

step_rotor(R = #{offset := Off}) -> R#{offset := (Off + 1) rem 26}.

make_plug([]) ->
    maps:from_list([{I, I} || I <- lists:seq(0, 25)]);
make_plug(Pairs) ->
    Base = make_plug([]),
    lists:foldl(fun([A0, B0], Acc) ->
        A = A0 - $A, B = B0 - $A,
        Acc#{A := B, B := A}
    end, Base, Pairs).

step_rotors(#{left := L, middle := M, right := R} = State) ->
    MidOff = maps:get(offset, M),
    MidNotch = maps:get(notch, M),
    RightOff = maps:get(offset, R),
    RightNotch = maps:get(notch, R),
    {L2, M2} = if
        MidOff =:= MidNotch -> {step_rotor(L), step_rotor(M)};
        RightOff =:= RightNotch -> {L, step_rotor(M)};
        true -> {L, M}
    end,
    R2 = step_rotor(R),
    State#{left := L2, middle := M2, right := R2}.

press_key(State, Char) ->
    S = step_rotors(State),
    #{left := L, middle := M, right := Ri, plug := Plug} = S,
    Idx0 = Char - $A,
    Idx1 = maps:get(Idx0, Plug),
    Idx2 = fwd_pass(Ri, Idx1),
    Idx3 = fwd_pass(M, Idx2),
    Idx4 = fwd_pass(L, Idx3),
    Idx5 = lists:nth(Idx4 + 1, refl()) - $A,
    Idx6 = bwd_pass(L, Idx5),
    Idx7 = bwd_pass(M, Idx6),
    Idx8 = bwd_pass(Ri, Idx7),
    Idx9 = maps:get(Idx8, Plug),
    {S, Idx9 + $A}.

encrypt(State, Text) ->
    Upper = string:to_upper(Text),
    Chars = [C || C <- Upper, C >= $A, C =< $Z],
    {_, Result} = lists:foldl(fun(C, {S, Acc}) ->
        {S2, Out} = press_key(S, C),
        {S2, [Out | Acc]}
    end, {State, []}, Chars),
    lists:reverse(Result).

new_machine([R1, R2, R3], [K1, K2, K3], Plugboard) ->
    #{left => make_rotor(R1, K1),
      middle => make_rotor(R2, K2),
      right => make_rotor(R3, K3),
      plug => make_plug(Plugboard)}.

main() ->
    io:format("Enigma Machine - Erlang Implementation~n"),
    io:format("=======================================~n"),
    Tests = [
        {[1,2,3], "AAA", [],              "AAAAA",        "BDZGO"},
        {[1,2,3], "AAA", [],              "HELLOWORLD",   "ILBDAAMTAZ"},
        {[1,2,3], "AAA", [],              "ATTACKATDAWN", "BZHGNOCRRTCM"},
        {[1,2,3], "MCK", [],              "HELLOWORLD",   "DLTBBQVPQV"},
        {[3,1,2], "AAA", [],              "HELLOWORLD",   "KZHDFQYHXT"},
        {[1,2,3], "AAA", ["AB","CD","EF"],"HELLOWORLD",   "IKACBBMTBF"}
    ],
    run_tests(Tests, 1, true).

run_tests([], _, AllPass) ->
    if AllPass -> io:format("~n  ALL 6 TESTS PASSED~n");
       true -> io:format("~n  SOME TESTS FAILED~n")
    end;
run_tests([{Rotors, Key, Plugs, Plain, Expected} | Rest], N, AllPass) ->
    M = new_machine(Rotors, Key, Plugs),
    Cipher = encrypt(M, Plain),
    Ok = Cipher =:= Expected,
    Status = if Ok -> "PASS"; true -> "FAIL" end,
    io:format("  Test ~p: ~s -> ~s [~s]~n", [N, Plain, Cipher, Status]),
    run_tests(Rest, N + 1, AllPass andalso Ok).
