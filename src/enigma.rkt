#lang racket
;; Enigma Machine - Racket Implementation
;; Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
;; PeopleTec Inc. - Guinness World Record Attempt 2026

(define fwd-wirings
  #("EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    "AJDKSIRUXBLHWTMCQGZNPYFVOE"
    "BDFHJLCPRTXVZNYEIWGAKMUSQO"))

(define bwd-wirings
  #("UWYGADFPVZBECKMTHXSLRINQOJ"
    "AJPCZWRLFBDKOTYUQGENHXMIVS"
    "TAGBPCSDQEUFVNZHYIXJWLRKOM"))

(define notches #(16 4 21))
(define reflector "YRUHQSLDPXNGOKMIEBFZCWVJAT")

(define (mod26 a) (modulo (+ (modulo a 26) 26) 26))
(define (c2i c) (- (char->integer c) (char->integer #\A)))
(define (i2c i) (integer->char (+ i (char->integer #\A))))

(struct rotor (fwd bwd notch [offset #:mutable]) #:transparent)

(define (make-rotor num win)
  (rotor (vector-ref fwd-wirings num)
         (vector-ref bwd-wirings num)
         (vector-ref notches num)
         (c2i win)))

(define (fwd-pass r idx)
  (let* ([contact (mod26 (+ idx (rotor-offset r)))]
         [out (c2i (string-ref (rotor-fwd r) contact))])
    (mod26 (- out (rotor-offset r)))))

(define (bwd-pass r idx)
  (let* ([contact (mod26 (+ idx (rotor-offset r)))]
         [out (c2i (string-ref (rotor-bwd r) contact))])
    (mod26 (- out (rotor-offset r)))))

(define (step! r)
  (set-rotor-offset! r (modulo (+ (rotor-offset r) 1) 26)))

(struct enigma (left middle right plug) #:transparent)

(define (make-enigma rotors key plugboard)
  (define plug (make-vector 26))
  (for ([i 26]) (vector-set! plug i i))
  (for ([pair plugboard])
    (let ([a (c2i (string-ref pair 0))]
          [b (c2i (string-ref pair 1))])
      (vector-set! plug a b)
      (vector-set! plug b a)))
  (enigma (make-rotor (list-ref rotors 0) (string-ref key 0))
          (make-rotor (list-ref rotors 1) (string-ref key 1))
          (make-rotor (list-ref rotors 2) (string-ref key 2))
          plug))

(define (step-rotors! e)
  (cond
    [(= (rotor-offset (enigma-middle e)) (rotor-notch (enigma-middle e)))
     (step! (enigma-middle e)) (step! (enigma-left e))]
    [(= (rotor-offset (enigma-right e)) (rotor-notch (enigma-right e)))
     (step! (enigma-middle e))])
  (step! (enigma-right e)))

(define (press-key e ch)
  (step-rotors! e)
  (let* ([idx (c2i ch)]
         [idx (vector-ref (enigma-plug e) idx)]
         [idx (fwd-pass (enigma-right e) idx)]
         [idx (fwd-pass (enigma-middle e) idx)]
         [idx (fwd-pass (enigma-left e) idx)]
         [idx (c2i (string-ref reflector idx))]
         [idx (bwd-pass (enigma-left e) idx)]
         [idx (bwd-pass (enigma-middle e) idx)]
         [idx (bwd-pass (enigma-right e) idx)]
         [idx (vector-ref (enigma-plug e) idx)])
    (i2c idx)))

(define (encrypt e text)
  (list->string
   (for/list ([c (string-upcase text)]
              #:when (char-alphabetic? c))
     (press-key e c))))

;; Test harness
(displayln "Enigma Machine - Racket Implementation")
(displayln "=======================================")

(define tests
  `(((0 1 2) "AAA" ()              "AAAAA"        "BDZGO")
    ((0 1 2) "AAA" ()              "HELLOWORLD"   "ILBDAAMTAZ")
    ((0 1 2) "AAA" ()              "ATTACKATDAWN" "BZHGNOCRRTCM")
    ((0 1 2) "MCK" ()              "HELLOWORLD"   "DLTBBQVPQV")
    ((2 0 1) "AAA" ()              "HELLOWORLD"   "KZHDFQYHXT")
    ((0 1 2) "AAA" ("AB" "CD" "EF") "HELLOWORLD" "IKACBBMTBF")))

(define all-pass #t)
(for ([t tests] [i (in-naturals 1)])
  (match-let ([(list rotors key plugs plain expected) t])
    (let* ([e (make-enigma rotors key plugs)]
           [cipher (encrypt e plain)]
           [ok (string=? cipher expected)]
           [status (if ok "PASS" "FAIL")])
      (printf "  Test ~a: ~a -> ~a [~a]\n" i plain cipher status)
      (unless ok
        (printf "          Expected ~a, got ~a\n" expected cipher)
        (set! all-pass #f)))))

(displayln (if all-pass "\n  ALL 6 TESTS PASSED" "\n  SOME TESTS FAILED"))
