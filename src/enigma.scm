;; Enigma Machine - Scheme Implementation (R7RS / Chicken Scheme)
;; Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
;; PeopleTec Inc. - Guinness World Record Attempt 2026

(define fwd-wirings
  (vector "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
          "AJDKSIRUXBLHWTMCQGZNPYFVOE"
          "BDFHJLCPRTXVZNYEIWGAKMUSQO"))

(define bwd-wirings
  (vector "UWYGADFPVZBECKMTHXSLRINQOJ"
          "AJPCZWRLFBDKOTYUQGENHXMIVS"
          "TAGBPCSDQEUFVNZHYIXJWLRKOM"))

(define notches (vector 16 4 21))
(define reflector "YRUHQSLDPXNGOKMIEBFZCWVJAT")

(define (mod26 a) (modulo (+ (modulo a 26) 26) 26))
(define (c2i c) (- (char->integer c) (char->integer #\A)))
(define (i2c i) (integer->char (+ i (char->integer #\A))))

(define (make-rotor num win)
  (vector (vector-ref fwd-wirings num)
          (vector-ref bwd-wirings num)
          (vector-ref notches num)
          (c2i win)))

(define (rotor-fwd r) (vector-ref r 0))
(define (rotor-bwd r) (vector-ref r 1))
(define (rotor-notch r) (vector-ref r 2))
(define (rotor-offset r) (vector-ref r 3))
(define (rotor-set-offset! r v) (vector-set! r 3 v))

(define (rotor-step! r)
  (rotor-set-offset! r (modulo (+ (rotor-offset r) 1) 26)))

(define (fwd-pass r idx)
  (let* ((contact (mod26 (+ idx (rotor-offset r))))
         (out (c2i (string-ref (rotor-fwd r) contact))))
    (mod26 (- out (rotor-offset r)))))

(define (bwd-pass r idx)
  (let* ((contact (mod26 (+ idx (rotor-offset r))))
         (out (c2i (string-ref (rotor-bwd r) contact))))
    (mod26 (- out (rotor-offset r)))))

(define (make-plug pairs)
  (let ((p (make-vector 26 0)))
    (do ((i 0 (+ i 1))) ((= i 26))
      (vector-set! p i i))
    (for-each (lambda (pair)
      (let ((a (c2i (string-ref pair 0)))
            (b (c2i (string-ref pair 1))))
        (vector-set! p a b)
        (vector-set! p b a)))
      pairs)
    p))

(define (make-enigma rotors key plugboard)
  (vector (make-rotor (list-ref rotors 0) (string-ref key 0))
          (make-rotor (list-ref rotors 1) (string-ref key 1))
          (make-rotor (list-ref rotors 2) (string-ref key 2))
          (make-plug plugboard)))

(define (enigma-left e) (vector-ref e 0))
(define (enigma-middle e) (vector-ref e 1))
(define (enigma-right e) (vector-ref e 2))
(define (enigma-plug e) (vector-ref e 3))

(define (step-rotors! e)
  (let ((mid (enigma-middle e))
        (right (enigma-right e))
        (left (enigma-left e)))
    (cond
      ((= (rotor-offset mid) (rotor-notch mid))
       (rotor-step! mid) (rotor-step! left))
      ((= (rotor-offset right) (rotor-notch right))
       (rotor-step! mid)))
    (rotor-step! right)))

(define (press-key e ch)
  (step-rotors! e)
  (let* ((idx (c2i ch))
         (idx (vector-ref (enigma-plug e) idx))
         (idx (fwd-pass (enigma-right e) idx))
         (idx (fwd-pass (enigma-middle e) idx))
         (idx (fwd-pass (enigma-left e) idx))
         (idx (c2i (string-ref reflector idx)))
         (idx (bwd-pass (enigma-left e) idx))
         (idx (bwd-pass (enigma-middle e) idx))
         (idx (bwd-pass (enigma-right e) idx))
         (idx (vector-ref (enigma-plug e) idx)))
    (i2c idx)))

(define (encrypt e text)
  (let ((upper (string-upcase text)))
    (list->string
      (map (lambda (c) (press-key e c))
           (filter (lambda (c) (and (char>=? c #\A) (char<=? c #\Z)))
                   (string->list upper))))))

;; Test harness
(display "Enigma Machine - Scheme Implementation\n")
(display "=======================================\n")

(define tests
  (list
    (list '(0 1 2) "AAA" '()              "AAAAA"        "BDZGO")
    (list '(0 1 2) "AAA" '()              "HELLOWORLD"   "ILBDAAMTAZ")
    (list '(0 1 2) "AAA" '()              "ATTACKATDAWN" "BZHGNOCRRTCM")
    (list '(0 1 2) "MCK" '()              "HELLOWORLD"   "DLTBBQVPQV")
    (list '(2 0 1) "AAA" '()              "HELLOWORLD"   "KZHDFQYHXT")
    (list '(0 1 2) "AAA" '("AB" "CD" "EF") "HELLOWORLD" "IKACBBMTBF")))

(define all-pass #t)

(let loop ((ts tests) (n 1))
  (when (not (null? ts))
    (let* ((t (car ts))
           (rotors (list-ref t 0))
           (key (list-ref t 1))
           (plugs (list-ref t 2))
           (plain (list-ref t 3))
           (expected (list-ref t 4))
           (e (make-enigma rotors key plugs))
           (cipher (encrypt e plain))
           (ok (string=? cipher expected))
           (status (if ok "PASS" "FAIL")))
      (display (string-append "  Test " (number->string n) ": "
                              plain " -> " cipher " [" status "]\n"))
      (when (not ok)
        (set! all-pass #f))
      (loop (cdr ts) (+ n 1)))))

(display (if all-pass "\n  ALL 6 TESTS PASSED\n" "\n  SOME TESTS FAILED\n"))
