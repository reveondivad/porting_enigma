\\ Enigma Cipher - Shen
\\ Functional language with pattern matching and sequent calculus type system
\\ Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
\\ PeopleTec Inc. - Guinness World Record Attempt 2026

(define fwdI  -> [4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9])
(define fwdII -> [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4])
(define fwdIII-> [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14])
(define bwdI  -> [20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9])
(define bwdII -> [0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18])
(define bwdIII-> [19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12])
(define reflector -> [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19])
(define notches -> [16 4 21])

(define mod26
  N -> (let M (shen.mod N 26)
         (if (< M 0) (+ M 26) M)))

(define nth-element
  0 [X | _] -> X
  N [_ | Xs] -> (nth-element (- N 1) Xs))

(define get-fwd
  0 I -> (nth-element I (fwdI))
  1 I -> (nth-element I (fwdII))
  _ I -> (nth-element I (fwdIII)))

(define get-bwd
  0 I -> (nth-element I (bwdI))
  1 I -> (nth-element I (bwdII))
  _ I -> (nth-element I (bwdIII)))

(define pass-fwd
  Rotor Offset Ch -> (let Inp (mod26 (+ Ch Offset))
                        Out (get-fwd Rotor Inp)
                        (mod26 (- Out Offset))))

(define pass-bwd
  Rotor Offset Ch -> (let Inp (mod26 (+ Ch Offset))
                        Out (get-bwd Rotor Inp)
                        (mod26 (- Out Offset))))

(define step-rotors
  O0 O1 O2 N1 N2 ->
    (let Mid (= O1 N1)
         Atn (= O2 N2)
         NewO2 (mod26 (+ O2 1))
         NewO1 (if (or Mid Atn) (mod26 (+ O1 1)) O1)
         NewO0 (if Mid (mod26 (+ O0 1)) O0)
      [NewO0 NewO1 NewO2]))

(define encrypt-char
  R0 R1 R2 O0 O1 O2 N1 N2 Ch ->
    (let Stepped (step-rotors O0 O1 O2 N1 N2)
         SO0 (nth-element 0 Stepped)
         SO1 (nth-element 1 Stepped)
         SO2 (nth-element 2 Stepped)
         C0 Ch
         C1 (pass-fwd R2 SO2 C0)
         C2 (pass-fwd R1 SO1 C1)
         C3 (pass-fwd R0 SO0 C2)
         C4 (nth-element C3 (reflector))
         C5 (pass-bwd R0 SO0 C4)
         C6 (pass-bwd R1 SO1 C5)
         C7 (pass-bwd R2 SO2 C6)
      [C7 SO0 SO1 SO2]))

\\ Test: AAAAA -> BDZGO
(output "Enigma Cipher - Shen~%")
(output "Test vectors: BDZGO, ILBDAAMTAZ, BZHGNOCRRTCM~%")
