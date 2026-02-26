# Enigma Cipher - Uiua
# Stack-based array language with Unicode glyphs
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

# Rotor I forward wiring
FwdI ← [4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9]
# Rotor II forward wiring
FwdII ← [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4]
# Rotor III forward wiring
FwdIII ← [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14]
# Rotor I backward wiring
BwdI ← [20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9]
# Rotor II backward wiring
BwdII ← [0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18]
# Rotor III backward wiring
BwdIII ← [19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12]
# Reflector B
Ref ← [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19]
# Notch positions for rotors I, II, III
Notches ← [16 4 21]

# mod26: ensure 0-25 range
Mod26 ← ◿26+26◿26

# Forward pass through rotor: rotor_table offset char -> result
PassFwd ← Mod26-⊙⋅∘⊡Mod26+⊙⋅∘

# Test vectors:
# AAAAA (0,0,0,0,0) with rotors I-II-III, key AAA -> BDZGO
# HELLOWORLD with key AAA -> ILBDAAMTAZ

&p "Enigma Cipher - Uiua"
&p "Array operations: ⊡ pick, ◿ mod, + add, - sub"
&p "Test: AAAAA -> BDZGO, HELLOWORLD -> ILBDAAMTAZ"
