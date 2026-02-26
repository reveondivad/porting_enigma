! Enigma Machine - Factor Implementation
! Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
! PeopleTec Inc. - Guinness World Record Attempt 2026

USING: arrays io kernel math math.parser sequences strings
       formatting vectors accessors locals ;
IN: enigma

CONSTANT: fwd-wirings { "EKMFLGDQVZNTOWYHXUSPAIBRCJ" "AJDKSIRUXBLHWTMCQGZNPYFVOE" "BDFHJLCPRTXVZNYEIWGAKMUSQO" }
CONSTANT: bwd-wirings { "UWYGADFPVZBECKMTHXSLRINQOJ" "AJPCZWRLFBDKOTYUQGENHXMIVS" "TAGBPCSDQEUFVNZHYIXJWLRKOM" }
CONSTANT: notch-pos { 16 4 21 }
CONSTANT: reflector "YRUHQSLDPXNGOKMIEBFZCWVJAT"

: mod26 ( a -- b ) 26 mod 26 + 26 mod ;
: c2i ( c -- n ) CHAR: A - ;
: i2c ( n -- c ) CHAR: A + ;

TUPLE: rotor fwd bwd notch offset ;

: <rotor> ( num win -- rotor )
    [ [ fwd-wirings nth ] [ bwd-wirings nth ] [ notch-pos nth ] tri ] dip
    c2i rotor boa ;

: fwd-pass ( rotor idx -- result )
    over offset>> + mod26
    over fwd>> nth c2i
    swap offset>> - mod26 ;

: bwd-pass ( rotor idx -- result )
    over offset>> + mod26
    over bwd>> nth c2i
    swap offset>> - mod26 ;

: step-rotor ( rotor -- ) dup offset>> 1 + 26 mod swap offset<< ;
: at-notch? ( rotor -- ? ) [ offset>> ] [ notch>> ] bi = ;

TUPLE: enigma left middle right plug ;

: <enigma> ( rotors key plugboard -- enigma )
    [let
        :> plugboard :> key :> rotors
        rotors first key first <rotor> :> left
        rotors second key second <rotor> :> middle
        rotors third key third <rotor> :> right
        26 <iota> >array :> plug
        plugboard [
            [ first c2i ] [ second c2i ] bi
            2dup plug set-nth
            swap plug set-nth
        ] each
        left middle right plug enigma boa
    ] ;

: step-rotors ( enigma -- )
    dup middle>> at-notch? [
        dup middle>> step-rotor
        dup left>> step-rotor
    ] [
        dup right>> at-notch? [
            dup middle>> step-rotor
        ] when
    ] if
    right>> step-rotor ;

: press-key ( enigma c -- out )
    over step-rotors
    c2i
    over plug>> nth
    over right>> swap fwd-pass
    over middle>> swap fwd-pass
    over left>> swap fwd-pass
    reflector nth c2i
    over left>> swap bwd-pass
    over middle>> swap bwd-pass
    over right>> swap bwd-pass
    swap plug>> nth
    i2c nip ;

: encrypt ( enigma text -- cipher )
    >upper [ CHAR: A CHAR: Z between? ] filter
    [ press-key ] with map >string ;

! Test harness
: run-tests ( -- )
    "Enigma Machine - Factor Implementation" print
    "=======================================" print

    {
        { { 0 1 2 } "AAA" { }                 "AAAAA"        "BDZGO" }
        { { 0 1 2 } "AAA" { }                 "HELLOWORLD"   "ILBDAAMTAZ" }
        { { 0 1 2 } "AAA" { }                 "ATTACKATDAWN" "BZHGNOCRRTCM" }
        { { 0 1 2 } "MCK" { }                 "HELLOWORLD"   "DLTBBQVPQV" }
        { { 2 0 1 } "AAA" { }                 "HELLOWORLD"   "KZHDFQYHXT" }
        { { 0 1 2 } "AAA" { "AB" "CD" "EF" }  "HELLOWORLD"   "IKACBBMTBF" }
    }

    t :> all-pass
    dup length <iota> [| i |
        i swap nth
        [ first3 <enigma> ] [ fourth ] [ 4 swap nth ] tri
        :> expected :> plain
        plain encrypt :> cipher
        cipher expected = :> ok
        ok [ "PASS" ] [ "FAIL" f all-pass! ] if :> status
        "  Test %d: %s -> %s [%s]\n" i 1 + plain cipher status 4array vprintf
        ok [ ] [ "          Expected %s\n" expected 1array vprintf ] if
    ] each drop
    all-pass
    [ "\n  ALL 6 TESTS PASSED" print ]
    [ "\n  SOME TESTS FAILED" print ] if ;

MAIN: run-tests
