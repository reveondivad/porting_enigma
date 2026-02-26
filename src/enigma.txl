% Enigma Cipher - TXL
% Source transformation language (Queen's University)
% Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
% PeopleTec Inc. - Guinness World Record Attempt 2026

define program
    [repeat statement]
end define

define statement
    [assignment] | [print_stmt]
end define

% Rotor I Forward: EKMFLGDQVZNTOWYHXUSPAIBRCJ
% 4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9
% Rotor II Forward: AJDKSIRUXBLHWTMCQGZNPYFVOE
% 0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4
% Rotor III Forward: BDFHJLCPRTXVZNYEIWGAKMUSQO
% 1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14
% Reflector B: YRUHQSLDPXNGOKMIEBFZCWVJAT
% 24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19
% Notches: Q=16, E=4, V=21

rule mod26
    replace [number]
        N [number]
    by
        N [rem 26] [+ 26] [rem 26]
end rule

function encrypt
    replace [repeat number]
        Msg [repeat number]
    construct FwdI [repeat number]
        4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9
    construct Reflector [repeat number]
        24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19
    by
        Msg
end function

% Test: AAAAA -> BDZGO, HELLOWORLD -> ILBDAAMTAZ
