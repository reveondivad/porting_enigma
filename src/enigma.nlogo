; Enigma cipher in NetLogo
globals [rotor-fwd-1 rotor-fwd-2 rotor-fwd-3 rotor-bwd-1 rotor-bwd-2 rotor-bwd-3 reflector-b notches pos result]

to setup
  set rotor-fwd-1 [4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9]
  set rotor-fwd-2 [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4]
  set rotor-fwd-3 [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14]
  set rotor-bwd-1 [20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9]
  set rotor-bwd-2 [0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18]
  set rotor-bwd-3 [19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12]
  set reflector-b [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19]
  set notches [16 4 21]
  set pos [0 0 0]
  set result ""
end

to-report mod26 [n]
  report ((n mod 26) + 26) mod 26
end

to-report rotor-pass [wiring c p]
  report mod26 (item (mod26 (c + p)) wiring - p)
end

to enigma [text]
  setup
  let i 0
  while [i < length text] [
    let ch (word-to-num item i text) - 65
    if ch >= 0 and ch < 26 [
      ; Step rotors
      let mid (item 1 pos = item 1 notches)
      if item 2 pos = item 2 notches [set pos replace-item 2 pos mod26 (item 2 pos + 1)]
      if mid or item 2 pos = item 2 notches [set pos replace-item 1 pos mod26 (item 1 pos + 1)]
      set pos replace-item 2 pos mod26 (item 2 pos + 1)
      ; Forward
      let c rotor-pass rotor-fwd-3 ch (item 2 pos)
      set c rotor-pass rotor-fwd-2 c (item 1 pos)
      set c rotor-pass rotor-fwd-1 c (item 0 pos)
      set c item c reflector-b
      ; Backward
      set c rotor-pass rotor-bwd-1 c (item 0 pos)
      set c rotor-pass rotor-bwd-2 c (item 1 pos)
      set c rotor-pass rotor-bwd-3 c (item 2 pos)
      set result (word result num-to-char (c + 65))
    ]
    set i i + 1
  ]
  print result
end
