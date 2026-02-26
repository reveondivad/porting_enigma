;; Enigma cipher in Pact (Kadena blockchain smart contract)
(module enigma GOV
  (defcap GOV () true)
  
  (defconst ROTOR-FWD-1 [4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9])
  (defconst ROTOR-FWD-2 [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4])
  (defconst ROTOR-FWD-3 [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14])
  (defconst REFLECTOR-B [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19])
  (defconst NOTCHES [16 4 21])

  (defun mod26:integer (n:integer)
    (mod (+ (mod n 26) 26) 26))

  (defun rotor-pass:integer (wiring:list c:integer pos:integer)
    (mod26 (- (at (mod26 (+ c pos)) wiring) pos)))

  (defun encrypt-char:integer (c:integer p0:integer p1:integer p2:integer)
    (let* ((c1 (rotor-pass ROTOR-FWD-3 c p2))
           (c2 (rotor-pass ROTOR-FWD-2 c1 p1))
           (c3 (rotor-pass ROTOR-FWD-1 c2 p0))
           (cr (at c3 REFLECTOR-B)))
      cr))
)
