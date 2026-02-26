;; Enigma cipher in CLIPS (C Language Integrated Production System)
(deftemplate enigma-config
  (multislot rotor-fwd-1)
  (multislot rotor-fwd-2)
  (multislot rotor-fwd-3)
  (multislot reflector)
  (multislot notches))

(deftemplate enigma-state
  (slot pos0 (type INTEGER) (default 0))
  (slot pos1 (type INTEGER) (default 0))
  (slot pos2 (type INTEGER) (default 0))
  (slot input-char (type INTEGER))
  (slot phase (type SYMBOL)))

(deffacts enigma-wirings
  (enigma-config
    (rotor-fwd-1 4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9)
    (rotor-fwd-2 0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4)
    (rotor-fwd-3 1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14)
    (reflector 24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19)
    (notches 16 4 21)))

(deffunction mod26 (?n)
  (mod (+ (mod ?n 26) 26) 26))

(deffunction nth-val (?mslot ?idx)
  (nth$ (+ ?idx 1) ?mslot))

(defrule step-rotors
  ?s <- (enigma-state (phase step) (pos0 ?p0) (pos1 ?p1) (pos2 ?p2))
  (enigma-config (notches $?notch))
  =>
  (bind ?mid (= ?p1 (nth-val ?notch 1)))
  (if (= ?p2 (nth-val ?notch 2)) then (bind ?p2 (mod26 (+ ?p2 1))))
  (if (or ?mid (= ?p2 (nth-val ?notch 2))) then (bind ?p1 (mod26 (+ ?p1 1))))
  (bind ?p2 (mod26 (+ ?p2 1)))
  (modify ?s (pos0 ?p0) (pos1 ?p1) (pos2 ?p2) (phase encrypt)))

(defrule encrypt-char
  ?s <- (enigma-state (phase encrypt) (input-char ?c) (pos0 ?p0) (pos1 ?p1) (pos2 ?p2))
  (enigma-config (rotor-fwd-1 $?rf1) (rotor-fwd-2 $?rf2) (rotor-fwd-3 $?rf3) (reflector $?ref))
  =>
  (bind ?c (mod26 (- (nth-val ?rf3 (mod26 (+ ?c ?p2))) ?p2)))
  (bind ?c (mod26 (- (nth-val ?rf2 (mod26 (+ ?c ?p1))) ?p1)))
  (bind ?c (mod26 (- (nth-val ?rf1 (mod26 (+ ?c ?p0))) ?p0)))
  (bind ?c (nth-val ?ref ?c))
  (printout t (+ ?c 65) crlf)
  (modify ?s (phase done)))
