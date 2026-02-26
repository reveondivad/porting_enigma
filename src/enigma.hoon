::  Enigma Cipher - Hoon (Urbit)
::  Functional language for the Urbit computing platform
::
|%
++  fwd-i   ^-  (list @ud)
  ~[4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9]
++  fwd-ii  ^-  (list @ud)
  ~[0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4]
++  fwd-iii ^-  (list @ud)
  ~[1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14]
::
++  bwd-i   ^-  (list @ud)
  ~[20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9]
++  bwd-ii  ^-  (list @ud)
  ~[0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18]
++  bwd-iii ^-  (list @ud)
  ~[19 0 6 1 15 2 18 3 16 4 20 9 21 13 25 7 24 8 23 5 22 11 17 12 14 10]
::
++  notches  ^-  (list @ud)  ~[16 4 21]
++  reflector-b  ^-  (list @ud)
  ~[24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19]
::
++  mod26
  |=  x=@sd
  ^-  @ud
  (mod (add (mod x 26) 26) 26)
::
++  snag-safe
  |=  [i=@ud l=(list @ud)]
  ^-  @ud
  (snag i l)
::
++  get-fwd
  |=  [r=@ud i=@ud]
  ^-  @ud
  ?:  =(r 0)  (snag-safe i fwd-i)
  ?:  =(r 1)  (snag-safe i fwd-ii)
  (snag-safe i fwd-iii)
::
++  get-bwd
  |=  [r=@ud i=@ud]
  ^-  @ud
  ?:  =(r 0)  (snag-safe i bwd-i)
  ?:  =(r 1)  (snag-safe i bwd-ii)
  (snag-safe i bwd-iii)
::
++  pass-fwd
  |=  [rotor=@ud offset=@ud ch=@ud]
  ^-  @ud
  =/  inp  (mod26 (add ch offset))
  =/  out  (get-fwd rotor inp)
  (mod26 (sub (add out 26) offset))
::
++  pass-bwd
  |=  [rotor=@ud offset=@ud ch=@ud]
  ^-  @ud
  =/  inp  (mod26 (add ch offset))
  =/  out  (get-bwd rotor inp)
  (mod26 (sub (add out 26) offset))
::
++  encrypt-step
  |=  $:  r0=@ud  r1=@ud  r2=@ud
          o0=@ud  o1=@ud  o2=@ud
          pb=(list @ud)  ch=@ud
      ==
  ^-  [@ud @ud @ud @ud]
  ::  step rotors
  =/  mid  =(o1 (snag-safe r1 notches))
  =/  atn  =(o2 (snag-safe r2 notches))
  =/  no2  (mod26 (add o2 1))
  =/  no1  ?:  |((atn) mid)  (mod26 (add o1 1))  o1
  =/  no0  ?:  mid  (mod26 (add o0 1))  o0
  ::  encrypt
  =/  c  (snag-safe ch pb)
  =/  c  (pass-fwd r2 no2 c)
  =/  c  (pass-fwd r1 no1 c)
  =/  c  (pass-fwd r0 no0 c)
  =/  c  (snag-safe c reflector-b)
  =/  c  (pass-bwd r0 no0 c)
  =/  c  (pass-bwd r1 no1 c)
  =/  c  (pass-bwd r2 no2 c)
  =/  c  (snag-safe c pb)
  [no0 no1 no2 c]
--
::  Test vectors:
::  AAAAA -> BDZGO
::  HELLOWORLD -> ILBDAAMTAZ
::  ATTACKATDAWN -> BZHGNOCRRTCM
::  HELLOWORLD (MCK) -> DLTBBQVPQV
::  HELLOWORLD (III-I-II) -> KZHDFQYHXT
::  HELLOWORLD (PB AB-CD-EF) -> IKACBBMTBF
