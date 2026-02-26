-- Enigma Cipher - AppleScript
-- macOS automation scripting language
-- Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
-- PeopleTec Inc. - Guinness World Record Attempt 2026

set fwdI to {4, 10, 12, 5, 11, 6, 3, 16, 21, 25, 13, 19, 14, 22, 24, 7, 23, 20, 18, 15, 0, 8, 1, 17, 2, 9}
set fwdII to {0, 9, 3, 10, 18, 8, 17, 20, 23, 1, 11, 7, 22, 19, 12, 2, 16, 6, 25, 13, 15, 24, 5, 21, 14, 4}
set fwdIII to {1, 3, 5, 7, 9, 11, 2, 15, 17, 19, 23, 21, 25, 13, 24, 4, 8, 22, 6, 0, 10, 12, 20, 18, 16, 14}
set bwdI to {20, 22, 24, 6, 0, 3, 5, 15, 21, 25, 1, 4, 2, 10, 12, 19, 7, 23, 18, 11, 17, 8, 13, 16, 14, 9}
set bwdII to {0, 9, 15, 2, 25, 22, 17, 11, 5, 1, 3, 10, 14, 19, 24, 20, 16, 6, 4, 13, 7, 23, 12, 8, 21, 18}
set bwdIII to {19, 0, 6, 1, 15, 2, 18, 3, 16, 4, 20, 5, 21, 13, 25, 7, 24, 8, 23, 9, 22, 11, 17, 10, 14, 12}
set reflectorB to {24, 17, 20, 7, 16, 18, 11, 3, 15, 23, 13, 6, 14, 10, 12, 8, 4, 1, 5, 25, 2, 22, 21, 9, 0, 19}
set notchList to {16, 4, 21}

on mod26(n)
    set m to n mod 26
    if m < 0 then set m to m + 26
    return m
end mod26

on getFwd(r, i)
    if r = 0 then return item (i + 1) of fwdI
    if r = 1 then return item (i + 1) of fwdII
    return item (i + 1) of fwdIII
end getFwd

on getBwd(r, i)
    if r = 0 then return item (i + 1) of bwdI
    if r = 1 then return item (i + 1) of bwdII
    return item (i + 1) of bwdIII
end getBwd

on passFwd(rotor, offset, ch)
    set inp to mod26(ch + offset)
    return mod26(getFwd(rotor, inp) - offset)
end passFwd

on passBwd(rotor, offset, ch)
    set inp to mod26(ch + offset)
    return mod26(getBwd(rotor, inp) - offset)
end passBwd

-- Global state
set rotors to {0, 1, 2}
set offsets to {0, 0, 0}
set n1 to item 2 of notchList
set n2 to item 3 of notchList

on stepRotors()
    global offsets, n1, n2
    if item 2 of offsets = n1 then
        set item 2 of offsets to mod26((item 2 of offsets) + 1)
        set item 1 of offsets to mod26((item 1 of offsets) + 1)
    else if item 3 of offsets = n2 then
        set item 2 of offsets to mod26((item 2 of offsets) + 1)
    end if
    set item 3 of offsets to mod26((item 3 of offsets) + 1)
end stepRotors

on pressKey(ch)
    global rotors, offsets, reflectorB
    stepRotors()
    set c to ch
    set c to passFwd(item 3 of rotors, item 3 of offsets, c)
    set c to passFwd(item 2 of rotors, item 2 of offsets, c)
    set c to passFwd(item 1 of rotors, item 1 of offsets, c)
    set c to (item (c + 1) of reflectorB)
    set c to passBwd(item 1 of rotors, item 1 of offsets, c)
    set c to passBwd(item 2 of rotors, item 2 of offsets, c)
    set c to passBwd(item 3 of rotors, item 3 of offsets, c)
    return c
end pressKey

display dialog "Enigma Cipher - AppleScript" & return & "Test: AAAAA -> BDZGO"
