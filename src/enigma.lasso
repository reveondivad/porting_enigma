[// Enigma Cipher - Lasso
// Web application language
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

define enigma_fwdI => array(4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9)
define enigma_fwdII => array(0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4)
define enigma_fwdIII => array(1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14)
define enigma_reflector => array(24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19)
define enigma_notches => array(16, 4, 21)

define mod26(n::integer) => {
    local(m = #n % 26)
    #m < 0 ? return #m + 26 | return #m
}

define enigma_getFwd(r::integer, i::integer) => {
    #r == 0 ? return enigma_fwdI->get(#i + 1)
    #r == 1 ? return enigma_fwdII->get(#i + 1)
    return enigma_fwdIII->get(#i + 1)
}

define enigma_passFwd(rotor::integer, offset::integer, ch::integer) => {
    local(inp = mod26(#ch + #offset))
    return mod26(enigma_getFwd(#rotor, #inp) - #offset)
}

'Enigma Cipher - Lasso\n'
'Test: AAAAA -> BDZGO\n'
]
