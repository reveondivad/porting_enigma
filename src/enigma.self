"Enigma Cipher - Self (prototype-based OOP from Sun Labs)
 Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
 PeopleTec Inc. - Guinness World Record Attempt 2026"

(|
  fwdI  = (4. 10. 12. 5. 11. 6. 3. 16. 21. 25. 13. 19. 14. 22. 24. 7. 23. 20. 18. 15. 0. 8. 1. 17. 2. 9).
  fwdII = (0. 9. 3. 10. 18. 8. 17. 20. 23. 1. 11. 7. 22. 19. 12. 2. 16. 6. 25. 13. 15. 24. 5. 21. 14. 4).
  fwdIII= (1. 3. 5. 7. 9. 11. 2. 15. 17. 19. 23. 21. 25. 13. 24. 4. 8. 22. 6. 0. 10. 12. 20. 18. 16. 14).
  bwdI  = (20. 22. 24. 6. 0. 3. 5. 15. 21. 25. 1. 4. 2. 10. 12. 19. 7. 23. 18. 11. 17. 8. 13. 16. 14. 9).
  bwdII = (0. 9. 15. 2. 25. 22. 17. 11. 5. 1. 3. 10. 14. 19. 24. 20. 16. 6. 4. 13. 7. 23. 12. 8. 21. 18).
  bwdIII= (19. 0. 6. 1. 15. 2. 18. 3. 16. 4. 20. 5. 21. 13. 25. 7. 24. 8. 23. 9. 22. 11. 17. 10. 14. 12).
  ref   = (24. 17. 20. 7. 16. 18. 11. 3. 15. 23. 13. 6. 14. 10. 12. 8. 4. 1. 5. 25. 2. 22. 21. 9. 0. 19).
  notches = (16. 4. 21).

  mod26: n = (| m | m: n % 26. m < 0 ifTrue: [m: m + 26]. m).

  getFwd: r At: i = (
    r = 0 ifTrue: [^ fwdI at: i].
    r = 1 ifTrue: [^ fwdII at: i].
    ^ fwdIII at: i
  ).

  getBwd: r At: i = (
    r = 0 ifTrue: [^ bwdI at: i].
    r = 1 ifTrue: [^ bwdII at: i].
    ^ bwdIII at: i
  ).

  passFwd: rotor Offset: offset Ch: ch = (|inp. out|
    inp: mod26: ch + offset.
    out: getFwd: rotor At: inp.
    mod26: out - offset
  ).

  passBwd: rotor Offset: offset Ch: ch = (|inp. out|
    inp: mod26: ch + offset.
    out: getBwd: rotor At: inp.
    mod26: out - offset
  ).

  enigmaMachine = (|
    parent* = traits clonable.
    r0 <- 0. r1 <- 1. r2 <- 2.
    o0 <- 0. o1 <- 0. o2 <- 0.
    n1 <- 4. n2 <- 21.
    pb <- (0. 1. 2. 3. 4. 5. 6. 7. 8. 9. 10. 11. 12. 13. 14. 15. 16. 17. 18. 19. 20. 21. 22. 23. 24. 25).

    step = (
      o1 = n1 ifTrue: [o1: mod26: o1 + 1. o0: mod26: o0 + 1]
               ifFalse: [o2 = n2 ifTrue: [o1: mod26: o1 + 1]].
      o2: mod26: o2 + 1
    ).

    pressKey: ch = (|c|
      step.
      c: pb at: ch.
      c: passFwd: r2 Offset: o2 Ch: c.
      c: passFwd: r1 Offset: o1 Ch: c.
      c: passFwd: r0 Offset: o0 Ch: c.
      c: ref at: c.
      c: passBwd: r0 Offset: o0 Ch: c.
      c: passBwd: r1 Offset: o1 Ch: c.
      c: passBwd: r2 Offset: o2 Ch: c.
      pb at: c
    ).
  |).
|)
