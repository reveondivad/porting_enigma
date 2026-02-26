#!/usr/bin/env tclsh
# Enigma Machine - Tcl Implementation
# Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
# PeopleTec Inc. - Guinness World Record Attempt 2026

set FWD(1) EKMFLGDQVZNTOWYHXUSPAIBRCJ
set FWD(2) AJDKSIRUXBLHWTMCQGZNPYFVOE
set FWD(3) BDFHJLCPRTXVZNYEIWGAKMUSQO
set BWD(1) UWYGADFPVZBECKMTHXSLRINQOJ
set BWD(2) AJPCZWRLFBDKOTYUQGENHXMIVS
set BWD(3) TAGBPCSDQEUFVNZHYIXJWLRKOM
set NOTCH(1) 16
set NOTCH(2) 4
set NOTCH(3) 21
set REFL YRUHQSLDPXNGOKMIEBFZCWVJAT

proc c2i {c} { expr {[scan $c %c] - 65} }
proc i2c {i} { format %c [expr {$i + 65}] }
proc mod26 {a} { expr {(($a % 26) + 26) % 26} }

proc init_enigma {r1 r2 r3 key plugboard} {
    global FWD BWD NOTCH
    global left_fwd left_bwd left_notch left_off
    global mid_fwd mid_bwd mid_notch mid_off
    global right_fwd right_bwd right_notch right_off
    global plug

    set left_fwd $FWD($r1); set left_bwd $BWD($r1); set left_notch $NOTCH($r1)
    set left_off [c2i [string index $key 0]]
    set mid_fwd $FWD($r2); set mid_bwd $BWD($r2); set mid_notch $NOTCH($r2)
    set mid_off [c2i [string index $key 1]]
    set right_fwd $FWD($r3); set right_bwd $BWD($r3); set right_notch $NOTCH($r3)
    set right_off [c2i [string index $key 2]]

    for {set i 0} {$i < 26} {incr i} { set plug($i) $i }
    foreach pair $plugboard {
        set a [c2i [string index $pair 0]]
        set b [c2i [string index $pair 1]]
        set plug($a) $b; set plug($b) $a
    }
}

proc fwd_pass {wiring off idx} {
    set contact [mod26 [expr {$idx + $off}]]
    set out [c2i [string index $wiring $contact]]
    return [mod26 [expr {$out - $off}]]
}

proc bwd_pass {wiring off idx} {
    set contact [mod26 [expr {$idx + $off}]]
    set out [c2i [string index $wiring $contact]]
    return [mod26 [expr {$out - $off}]]
}

proc step_rotors {} {
    global left_off mid_off right_off mid_notch right_notch
    if {$mid_off == $mid_notch} {
        set mid_off [expr {($mid_off + 1) % 26}]
        set left_off [expr {($left_off + 1) % 26}]
    } elseif {$right_off == $right_notch} {
        set mid_off [expr {($mid_off + 1) % 26}]
    }
    set right_off [expr {($right_off + 1) % 26}]
}

proc press_key {c} {
    global left_fwd left_bwd left_off mid_fwd mid_bwd mid_off
    global right_fwd right_bwd right_off plug REFL
    step_rotors
    set idx [c2i $c]
    set idx $plug($idx)
    set idx [fwd_pass $right_fwd $right_off $idx]
    set idx [fwd_pass $mid_fwd $mid_off $idx]
    set idx [fwd_pass $left_fwd $left_off $idx]
    set idx [c2i [string index $REFL $idx]]
    set idx [bwd_pass $left_bwd $left_off $idx]
    set idx [bwd_pass $mid_bwd $mid_off $idx]
    set idx [bwd_pass $right_bwd $right_off $idx]
    set idx $plug($idx)
    return [i2c $idx]
}

proc encrypt {text} {
    set text [string toupper $text]
    set result ""
    for {set i 0} {$i < [string length $text]} {incr i} {
        set c [string index $text $i]
        if {[string match {[A-Z]} $c]} {
            append result [press_key $c]
        }
    }
    return $result
}

puts "Enigma Machine - Tcl Implementation"
puts "===================================="

set tests {
    {1 2 3 AAA {} AAAAA BDZGO}
    {1 2 3 AAA {} HELLOWORLD ILBDAAMTAZ}
    {1 2 3 AAA {} ATTACKATDAWN BZHGNOCRRTCM}
    {1 2 3 MCK {} HELLOWORLD DLTBBQVPQV}
    {3 1 2 AAA {} HELLOWORLD KZHDFQYHXT}
    {1 2 3 AAA {AB CD EF} HELLOWORLD IKACBBMTBF}
}

set all_pass 1
set num 1
foreach test $tests {
    lassign $test r1 r2 r3 key plugs plain expected
    init_enigma $r1 $r2 $r3 $key $plugs
    set cipher [encrypt $plain]
    if {$cipher eq $expected} {
        puts [format "  Test %d: %-20s -> %-15s \[PASS\]" $num $plain $cipher]
    } else {
        puts [format "  Test %d: %-20s -> %-15s \[FAIL\] expected %s" $num $plain $cipher $expected]
        set all_pass 0
    }
    incr num
}
puts [expr {$all_pass ? "\n  ALL 6 TESTS PASSED" : "\n  SOME TESTS FAILED"}]
