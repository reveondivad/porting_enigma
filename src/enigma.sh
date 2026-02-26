#!/usr/bin/env bash
# Enigma Machine - Bash Implementation
# Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
# PeopleTec Inc. - Guinness World Record Attempt 2026

FWD_1="EKMFLGDQVZNTOWYHXUSPAIBRCJ"
FWD_2="AJDKSIRUXBLHWTMCQGZNPYFVOE"
FWD_3="BDFHJLCPRTXVZNYEIWGAKMUSQO"
BWD_1="UWYGADFPVZBECKMTHXSLRINQOJ"
BWD_2="AJPCZWRLFBDKOTYUQGENHXMIVS"
BWD_3="TAGBPCSDQEUFVNZHYIXJWLRKOM"
NOTCH_1=16  # Q
NOTCH_2=4   # E
NOTCH_3=21  # V
REFL="YRUHQSLDPXNGOKMIEBFZCWVJAT"

# State variables
LEFT_FWD="" ; LEFT_BWD="" ; LEFT_NOTCH=0 ; LEFT_OFF=0
MID_FWD=""  ; MID_BWD=""  ; MID_NOTCH=0  ; MID_OFF=0
RIGHT_FWD="" ; RIGHT_BWD="" ; RIGHT_NOTCH=0 ; RIGHT_OFF=0
declare -a PLUG

c2i() { printf '%d' $(( $(printf '%d' "'$1") - 65 )); }

mod26() { echo $(( (($1 % 26) + 26) % 26 )); }

init_rotor() {
    local num=$1 win=$2
    local off=$(c2i "$win")
    case $num in
        1) eval "${3}_FWD=\$FWD_1; ${3}_BWD=\$BWD_1; ${3}_NOTCH=\$NOTCH_1; ${3}_OFF=$off" ;;
        2) eval "${3}_FWD=\$FWD_2; ${3}_BWD=\$BWD_2; ${3}_NOTCH=\$NOTCH_2; ${3}_OFF=$off" ;;
        3) eval "${3}_FWD=\$FWD_3; ${3}_BWD=\$BWD_3; ${3}_NOTCH=\$NOTCH_3; ${3}_OFF=$off" ;;
    esac
}

fwd_pass() {
    local wiring=$1 offset=$2 idx=$3
    local contact=$(mod26 $(( idx + offset )))
    local ch="${wiring:$contact:1}"
    local out=$(c2i "$ch")
    mod26 $(( out - offset ))
}

bwd_pass() {
    local wiring=$1 offset=$2 idx=$3
    local contact=$(mod26 $(( idx + offset )))
    local ch="${wiring:$contact:1}"
    local out=$(c2i "$ch")
    mod26 $(( out - offset ))
}

init_enigma() {
    local r1=$1 r2=$2 r3=$3 k1=$4 k2=$5 k3=$6
    shift 6
    init_rotor "$r1" "$k1" LEFT
    init_rotor "$r2" "$k2" MID
    init_rotor "$r3" "$k3" RIGHT
    for i in $(seq 0 25); do PLUG[$i]=$i; done
    while [ $# -ge 1 ]; do
        local pair=$1; shift
        local a=$(c2i "${pair:0:1}")
        local b=$(c2i "${pair:1:1}")
        PLUG[$a]=$b; PLUG[$b]=$a
    done
}

step_rotors() {
    if [ $MID_OFF -eq $MID_NOTCH ]; then
        MID_OFF=$(( (MID_OFF + 1) % 26 ))
        LEFT_OFF=$(( (LEFT_OFF + 1) % 26 ))
    elif [ $RIGHT_OFF -eq $RIGHT_NOTCH ]; then
        MID_OFF=$(( (MID_OFF + 1) % 26 ))
    fi
    RIGHT_OFF=$(( (RIGHT_OFF + 1) % 26 ))
}

press_key() {
    local ch=$1
    step_rotors
    local idx=$(c2i "$ch")
    idx=${PLUG[$idx]}
    idx=$(fwd_pass "$RIGHT_FWD" $RIGHT_OFF $idx)
    idx=$(fwd_pass "$MID_FWD" $MID_OFF $idx)
    idx=$(fwd_pass "$LEFT_FWD" $LEFT_OFF $idx)
    local rc="${REFL:$idx:1}"
    idx=$(c2i "$rc")
    idx=$(bwd_pass "$LEFT_BWD" $LEFT_OFF $idx)
    idx=$(bwd_pass "$MID_BWD" $MID_OFF $idx)
    idx=$(bwd_pass "$RIGHT_BWD" $RIGHT_OFF $idx)
    idx=${PLUG[$idx]}
    printf "\\$(printf '%03o' $(( idx + 65 )))"
}

encrypt() {
    local text="${1^^}"  # uppercase
    local result=""
    for (( i=0; i<${#text}; i++ )); do
        local c="${text:$i:1}"
        if [[ "$c" =~ [A-Z] ]]; then
            result+=$(press_key "$c")
        fi
    done
    echo "$result"
}

run_test() {
    local num=$1 r1=$2 r2=$3 r3=$4 k=$5 plain=$6 expected=$7
    shift 7
    init_enigma "$r1" "$r2" "$r3" "${k:0:1}" "${k:1:1}" "${k:2:1}" "$@"
    local cipher=$(encrypt "$plain")
    if [ "$cipher" = "$expected" ]; then
        printf "  Test %d: %-20s -> %-15s [PASS]\n" "$num" "$plain" "$cipher"
    else
        printf "  Test %d: %-20s -> %-15s [FAIL] expected %s\n" "$num" "$plain" "$cipher" "$expected"
        ALL_PASS=0
    fi
}

echo "Enigma Machine - Bash Implementation"
echo "====================================="

ALL_PASS=1
run_test 1 1 2 3 AAA AAAAA BDZGO
run_test 2 1 2 3 AAA HELLOWORLD ILBDAAMTAZ
run_test 3 1 2 3 AAA ATTACKATDAWN BZHGNOCRRTCM
run_test 4 1 2 3 MCK HELLOWORLD DLTBBQVPQV
run_test 5 3 1 2 AAA HELLOWORLD KZHDFQYHXT
run_test 6 1 2 3 AAA HELLOWORLD IKACBBMTBF AB CD EF

if [ $ALL_PASS -eq 1 ]; then
    echo "  ALL 6 TESTS PASSED"
else
    echo "  SOME TESTS FAILED"
fi
