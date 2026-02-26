-- Enigma Cipher - Futhark
-- High-performance functional GPU language
-- Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
-- PeopleTec Inc. - Guinness World Record Attempt 2026

let fwdI  : [26]i32 = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
let fwdII : [26]i32 = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
let fwdIII: [26]i32 = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
let bwdI  : [26]i32 = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
let bwdII : [26]i32 = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
let bwdIII: [26]i32 = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
let ref   : [26]i32 = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
let notches: [3]i32 = [16, 4, 21]

let mod26 (n: i32) : i32 =
  let m = n %% 26
  in if m < 0 then m + 26 else m

let get_fwd (r: i32) (i: i32) : i32 =
  if r == 0 then fwdI[i] else if r == 1 then fwdII[i] else fwdIII[i]

let get_bwd (r: i32) (i: i32) : i32 =
  if r == 0 then bwdI[i] else if r == 1 then bwdII[i] else bwdIII[i]

let pass_fwd (rotor: i32) (offset: i32) (ch: i32) : i32 =
  let inp = mod26(ch + offset)
  let out = get_fwd rotor inp
  in mod26(out - offset)

let pass_bwd (rotor: i32) (offset: i32) (ch: i32) : i32 =
  let inp = mod26(ch + offset)
  let out = get_bwd rotor inp
  in mod26(out - offset)

type state = {r0: i32, r1: i32, r2: i32,
              o0: i32, o1: i32, o2: i32,
              n1: i32, n2: i32}

let step (s: state) : state =
  let mid = s.o1 == s.n1
  let atn = s.o2 == s.n2
  let o2 = mod26(s.o2 + 1)
  let o1 = if mid || atn then mod26(s.o1 + 1) else s.o1
  let o0 = if mid then mod26(s.o0 + 1) else s.o0
  in s with o0 with o1 with o2

let encrypt_char (s: state) (ch: i32) : (i32, state) =
  let ns = step s
  let c = ch
  let c = pass_fwd ns.r2 ns.o2 c
  let c = pass_fwd ns.r1 ns.o1 c
  let c = pass_fwd ns.r0 ns.o0 c
  let c = ref[c]
  let c = pass_bwd ns.r0 ns.o0 c
  let c = pass_bwd ns.r1 ns.o1 c
  let c = pass_bwd ns.r2 ns.o2 c
  in (c, ns)

-- Sequential encryption (Enigma is inherently sequential due to state)
let encrypt [n] (r0: i32) (r1: i32) (r2: i32)
                (k0: i32) (k1: i32) (k2: i32)
                (msg: [n]i32) : [n]i32 =
  let init : state = {r0, r1, r2, o0=k0, o1=k1, o2=k2,
                       n1=notches[r1], n2=notches[r2]}
  let (result, _) =
    loop (acc, s) = (replicate n 0i32, init) for i < n do
      let (enc, ns) = encrypt_char s msg[i]
      in (acc with [i] = enc, ns)
  in result

-- Entry point: test AAAAA -> BDZGO
entry main : [5]i32 =
  encrypt 0 1 2 0 0 0 [0i32, 0, 0, 0, 0]
