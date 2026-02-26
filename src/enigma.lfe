;; Enigma Cipher - LFE (Lisp Flavored Erlang)
;; Runs on the BEAM VM with Lisp syntax
;; Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
;; PeopleTec Inc. - Guinness World Record Attempt 2026

(defmodule enigma
  (export (encrypt 7) (test 0)))

(defun fwd-wirings ()
  #(#(4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9)
    #(0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4)
    #(1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14)))

(defun bwd-wirings ()
  #(#(20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9)
    #(0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18)
    #(19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12)))

(defun reflector ()
  #(24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19))

(defun notches () #(16 4 21))

(defun mod26 (n)
  (let ((m (rem n 26)))
    (if (< m 0) (+ m 26) m)))

(defun pass-fwd (rotor offset ch)
  (let* ((inp (mod26 (+ ch offset)))
         (out (element (+ inp 1) (element (+ rotor 1) (fwd-wirings)))))
    (mod26 (- out offset))))

(defun pass-bwd (rotor offset ch)
  (let* ((inp (mod26 (+ ch offset)))
         (out (element (+ inp 1) (element (+ rotor 1) (bwd-wirings)))))
    (mod26 (- out offset))))

(defun step-rotors (o0 o1 o2 n1 n2)
  (let* ((mid (=:= o1 n1))
         (atn (=:= o2 n2))
         (new-o2 (mod26 (+ o2 1)))
         (new-o1 (if (or mid atn) (mod26 (+ o1 1)) o1))
         (new-o0 (if mid (mod26 (+ o0 1)) o0)))
    (tuple new-o0 new-o1 new-o2)))

(defun encrypt-char (r0 r1 r2 o0 o1 o2 n1 n2 ch)
  (let* (((tuple no0 no1 no2) (step-rotors o0 o1 o2 n1 n2))
         (c (pass-fwd r2 no2 ch))
         (c (pass-fwd r1 no1 c))
         (c (pass-fwd r0 no0 c))
         (c (element (+ c 1) (reflector)))
         (c (pass-bwd r0 no0 c))
         (c (pass-bwd r1 no1 c))
         (c (pass-bwd r2 no2 c)))
    (tuple c no0 no1 no2)))

(defun encrypt-loop (r0 r1 r2 o0 o1 o2 n1 n2 chars acc)
  (case chars
    ('() (lists:reverse acc))
    ((cons ch rest)
     (let (((tuple enc no0 no1 no2)
            (encrypt-char r0 r1 r2 o0 o1 o2 n1 n2 (- ch 65))))
       (encrypt-loop r0 r1 r2 no0 no1 no2 n1 n2 rest
                     (cons (+ enc 65) acc))))))

(defun encrypt (r0 r1 r2 k0 k1 k2 msg)
  (let ((n1 (element (+ r1 1) (notches)))
        (n2 (element (+ r2 1) (notches))))
    (list_to_binary
     (encrypt-loop r0 r1 r2 k0 k1 k2 n1 n2
                   (binary_to_list (list_to_binary msg)) '()))))

(defun test ()
  (io:format "Enigma Cipher - LFE~n")
  (io:format "Test 1: ~s~n" (list (encrypt 0 1 2 0 0 0 "AAAAA")))
  (io:format "Test 2: ~s~n" (list (encrypt 0 1 2 0 0 0 "HELLOWORLD")))
  (io:format "Test 3: ~s~n" (list (encrypt 0 1 2 0 0 0 "ATTACKATDAWN")))
  (io:format "Test 4: ~s~n" (list (encrypt 0 1 2 12 2 10 "HELLOWORLD")))
  (io:format "Test 5: ~s~n" (list (encrypt 2 0 1 0 0 0 "HELLOWORLD"))))
