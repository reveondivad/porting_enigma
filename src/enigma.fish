#!/usr/bin/env fish
# Enigma cipher in Fish shell
set rotor_fwd_0 4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9
set rotor_fwd_1 0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4
set rotor_fwd_2 1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14
set rotor_bwd_0 20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9
set rotor_bwd_1 0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18
set rotor_bwd_2 19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12
set reflector 24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19
set notches 16 4 21
set pos 0 0 0

function mod26
    set -l n $argv[1]
    math "((($n) % 26) + 26) % 26"
end

function enigma
    set -l text (string upper $argv[1])
    set -l result ""
    for ch in (string split "" $text)
        set -l code (printf "%d" "'$ch")
        if test $code -lt 65 -o $code -gt 90; continue; end
        set -l mid 0
        if test $pos[2] -eq $notches[2]; set mid 1; end
        if test $pos[3] -eq $notches[3]; set pos[3] (mod26 (math "$pos[3]+1")); end
        if test $mid -eq 1 -o $pos[3] -eq $notches[3]; set pos[2] (mod26 (math "$pos[2]+1")); end
        set pos[3] (mod26 (math "$pos[3]+1"))
        set -l c (math "$code - 65")
        for i in 2 1 0
            set -l idx (math "(mod26 (math '$c + $pos['(math $i+1)']')) + 1")
            set -l varname rotor_fwd_$i
            set c (mod26 (math "$$varname[$idx] - $pos["(math $i+1)"]"))
        end
        set c $reflector[(math "$c + 1")]
        for i in 0 1 2
            set -l idx (math "(mod26 (math '$c + $pos['(math $i+1)']')) + 1")
            set -l varname rotor_bwd_$i
            set c (mod26 (math "$$varname[$idx] - $pos["(math $i+1)"]"))
        end
        set result $result(printf "\\x"(printf "%02x" (math "$c + 65")))
    end
    echo $result
end

enigma "HELLOWORLD"
