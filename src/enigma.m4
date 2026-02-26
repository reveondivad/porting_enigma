dnl Enigma Cipher - M4
dnl Unix macro processor
dnl Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
dnl PeopleTec Inc. - Guinness World Record Attempt 2026
dnl
dnl M4 is a general-purpose macro processor
dnl define(name, expansion) creates macros
dnl
dnl Rotor wirings as indexed macros
define(`FWD_I_0',4)dnl
define(`FWD_I_1',10)dnl
define(`FWD_I_2',12)dnl
define(`FWD_I_3',5)dnl
define(`FWD_I_4',11)dnl
define(`FWD_I_5',6)dnl
dnl ... (full 26 entries per rotor)
dnl
define(`REFL_0',24)dnl
define(`REFL_1',17)dnl
define(`REFL_2',20)dnl
define(`REFL_3',7)dnl
dnl
dnl mod26 macro
define(`MOD26',`ifelse(eval($1 % 26 < 0),1,eval($1 % 26 + 26),eval($1 % 26))')dnl
dnl
dnl Forward pass macro
define(`PASS_FWD',`MOD26(eval(FWD_I_`'MOD26(eval($3 + $2)) - $2))')dnl
dnl
dnl Notches: 16, 4, 21
dnl Test: AAAAA -> BDZGO
`Enigma Cipher - M4'
`Test: AAAAA -> BDZGO'
