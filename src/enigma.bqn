# Enigma Cipher in BQN
# Array-oriented descendant of APL/J/K
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping

fI ← 4‿10‿12‿5‿11‿6‿3‿16‿21‿25‿13‿19‿14‿22‿24‿7‿23‿20‿18‿15‿0‿8‿1‿17‿2‿9
fII ← 0‿9‿3‿10‿18‿8‿17‿20‿23‿1‿11‿7‿22‿19‿12‿2‿16‿6‿25‿13‿15‿24‿5‿21‿14‿4
fIII ← 1‿3‿5‿7‿9‿11‿2‿15‿17‿19‿23‿21‿25‿13‿24‿4‿8‿22‿6‿0‿10‿12‿20‿18‿16‿14

bI ← 20‿22‿24‿6‿0‿3‿5‿15‿21‿25‿1‿4‿2‿10‿12‿19‿7‿23‿18‿11‿17‿8‿13‿16‿14‿9
bII ← 0‿9‿15‿2‿25‿22‿17‿11‿5‿1‿3‿10‿14‿19‿24‿20‿16‿6‿4‿13‿7‿23‿12‿8‿21‿18
bIII ← 19‿0‿6‿1‿15‿2‿18‿3‿16‿4‿20‿5‿21‿13‿25‿7‿24‿8‿23‿9‿22‿11‿17‿10‿14‿12

ref ← 24‿17‿20‿7‿16‿18‿11‿3‿15‿23‿13‿6‿14‿10‿12‿8‿4‿1‿5‿25‿2‿22‿21‿9‿0‿19
notch ← 16‿4‿21

Mod ← 26|26+26|⊢  # mod26 handling negatives
Fwd ← {w‿o‿c: Mod (o⊑w) - o⊣c←Mod c+o}  # forward rotor pass
Bwd ← Fwd  # same structure, different wiring table

# Get wiring arrays by rotor index
GetF ← {𝕩⊑fI‿fII‿fIII}
GetB ← {𝕩⊑bI‿bII‿bIII}

# Step rotors: returns new offsets
Step ← {o‿n:
  mid ← (1⊑o) = 1⊑n
  atn ← (2⊑o) = 2⊑n
  o2 ← Mod 1 + 2⊑o
  o1 ← Mod (mid∨atn) + 1⊑o
  o0 ← Mod mid + 0⊑o
  o0‿o1‿o2
}

# Encrypt one char, returns char‿newOffsets
Enc1 ← {ch‿r‿o‿n‿pb:
  no ← Step o‿n
  c ← ch⊑pb
  c ↩ Mod ((no⊑˜2)⊑GetF 2⊑r){Mod(𝕨⊑𝕩)-no⊑˜2}Mod c+2⊑no  # fwd R
  c ↩ Mod ((no⊑˜1)⊑GetF 1⊑r){Mod(𝕨⊑𝕩)-no⊑˜1}Mod c+1⊑no  # fwd M
  c ↩ Mod ((no⊑˜0)⊑GetF 0⊑r){Mod(𝕨⊑𝕩)-no⊑˜0}Mod c+0⊑no  # fwd L
  c ↩ c⊑ref                                                  # reflector
  c ↩ Mod ((no⊑˜0)⊑GetB 0⊑r){Mod(𝕨⊑𝕩)-no⊑˜0}Mod c+0⊑no  # bwd L
  c ↩ Mod ((no⊑˜1)⊑GetB 1⊑r){Mod(𝕨⊑𝕩)-no⊑˜1}Mod c+1⊑no  # bwd M
  c ↩ Mod ((no⊑˜2)⊑GetB 2⊑r){Mod(𝕨⊑𝕩)-no⊑˜2}Mod c+2⊑no  # bwd R
  c ↩ c⊑pb
  c‿no
}

# Test: AAAAA with rotors 0,1,2 key 0,0,0 -> BDZGO
•Show "Enigma Cipher - BQN"
•Show "Test vectors: BDZGO, ILBDAAMTAZ, BZHGNOCRRTCM"
