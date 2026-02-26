; Enigma cipher in SMT-LIB (satisfiability modulo theories)
(set-logic QF_LIA)

; Rotor I forward wiring
(define-fun rf1 ((x Int)) Int
  (ite (= x 0) 4 (ite (= x 1) 10 (ite (= x 2) 12 (ite (= x 3) 5
  (ite (= x 4) 11 (ite (= x 5) 6 (ite (= x 6) 3 (ite (= x 7) 16
  (ite (= x 8) 21 (ite (= x 9) 25 (ite (= x 10) 13 (ite (= x 11) 19
  (ite (= x 12) 14 (ite (= x 13) 22 (ite (= x 14) 24 (ite (= x 15) 7
  (ite (= x 16) 23 (ite (= x 17) 20 (ite (= x 18) 18 (ite (= x 19) 15
  (ite (= x 20) 0 (ite (= x 21) 8 (ite (= x 22) 1 (ite (= x 23) 17
  (ite (= x 24) 2 9))))))))))))))))))))))))))

; Reflector B
(define-fun refB ((x Int)) Int
  (ite (= x 0) 24 (ite (= x 1) 17 (ite (= x 2) 20 (ite (= x 3) 7
  (ite (= x 4) 16 (ite (= x 5) 18 (ite (= x 6) 11 (ite (= x 7) 3
  (ite (= x 8) 15 (ite (= x 9) 23 (ite (= x 10) 13 (ite (= x 11) 6
  (ite (= x 12) 14 (ite (= x 13) 10 (ite (= x 14) 12 (ite (= x 15) 8
  (ite (= x 16) 4 (ite (= x 17) 1 (ite (= x 18) 5 (ite (= x 19) 25
  (ite (= x 20) 2 (ite (= x 21) 22 (ite (= x 22) 21 (ite (= x 23) 9
  (ite (= x 24) 0 19))))))))))))))))))))))))))

(define-fun mod26 ((n Int)) Int
  (mod (+ (mod n 26) 26) 26))

; Verify reflector is involutory: refB(refB(x)) = x for all x in 0..25
(declare-const x Int)
(assert (and (>= x 0) (<= x 25)))
(assert (not (= (refB (refB x)) x)))
(check-sat) ; Should be UNSAT (proving involution)
