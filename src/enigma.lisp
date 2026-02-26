;;; Enigma Machine - Common Lisp Implementation
;;; Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
;;; PeopleTec Inc. - Guinness World Record Attempt 2026

(defparameter *fwd*
  #("EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    "AJDKSIRUXBLHWTMCQGZNPYFVOE"
    "BDFHJLCPRTXVZNYEIWGAKMUSQO"))

(defparameter *bwd*
  #("UWYGADFPVZBECKMTHXSLRINQOJ"
    "AJPCZWRLFBDKOTYUQGENHXMIVS"
    "TAGBPCSDQEUFVNZHYIXJWLRKOM"))

(defparameter *notches* #(16 4 21))
(defparameter *refl* "YRUHQSLDPXNGOKMIEBFZCWVJAT")

(defun mod26 (a) (mod (+ (mod a 26) 26) 26))
(defun c2i (c) (- (char-code c) (char-code #\A)))
(defun i2c (i) (code-char (+ i (char-code #\A))))

(defstruct rotor fwd bwd notch offset)

(defun make-rotor-obj (num win)
  (make-rotor :fwd (aref *fwd* num) :bwd (aref *bwd* num)
              :notch (aref *notches* num) :offset (c2i win)))

(defun fwd-pass (r idx)
  (let* ((contact (mod26 (+ idx (rotor-offset r))))
         (out (c2i (char (rotor-fwd r) contact))))
    (mod26 (- out (rotor-offset r)))))

(defun bwd-pass (r idx)
  (let* ((contact (mod26 (+ idx (rotor-offset r))))
         (out (c2i (char (rotor-bwd r) contact))))
    (mod26 (- out (rotor-offset r)))))

(defun step-rotor (r)
  (setf (rotor-offset r) (mod (1+ (rotor-offset r)) 26)))

(defstruct enigma left middle right plug)

(defun make-enigma-obj (rotors key plugboard)
  (let ((plug (make-array 26 :initial-contents (loop for i from 0 below 26 collect i))))
    (dolist (pair plugboard)
      (let ((a (c2i (char pair 0))) (b (c2i (char pair 1))))
        (setf (aref plug a) b (aref plug b) a)))
    (make-enigma
     :left (make-rotor-obj (first rotors) (char key 0))
     :middle (make-rotor-obj (second rotors) (char key 1))
     :right (make-rotor-obj (third rotors) (char key 2))
     :plug plug)))

(defun step-rotors (e)
  (let ((mid (enigma-middle e))
        (right (enigma-right e))
        (left (enigma-left e)))
    (cond
      ((= (rotor-offset mid) (rotor-notch mid))
       (step-rotor mid) (step-rotor left))
      ((= (rotor-offset right) (rotor-notch right))
       (step-rotor mid)))
    (step-rotor right)))

(defun press-key (e ch)
  (step-rotors e)
  (let ((idx (c2i ch)))
    (setf idx (aref (enigma-plug e) idx))
    (setf idx (fwd-pass (enigma-right e) idx))
    (setf idx (fwd-pass (enigma-middle e) idx))
    (setf idx (fwd-pass (enigma-left e) idx))
    (setf idx (c2i (char *refl* idx)))
    (setf idx (bwd-pass (enigma-left e) idx))
    (setf idx (bwd-pass (enigma-middle e) idx))
    (setf idx (bwd-pass (enigma-right e) idx))
    (setf idx (aref (enigma-plug e) idx))
    (i2c idx)))

(defun encrypt (e text)
  (coerce
   (loop for c across (string-upcase text)
         when (alpha-char-p c) collect (press-key e c))
   'string))

(defun main ()
  (format t "Enigma Machine - Common Lisp Implementation~%")
  (format t "=============================================~%")
  (let ((tests '(((0 1 2) "AAA" ()              "AAAAA"        "BDZGO")
                 ((0 1 2) "AAA" ()              "HELLOWORLD"   "ILBDAAMTAZ")
                 ((0 1 2) "AAA" ()              "ATTACKATDAWN" "BZHGNOCRRTCM")
                 ((0 1 2) "MCK" ()              "HELLOWORLD"   "DLTBBQVPQV")
                 ((2 0 1) "AAA" ()              "HELLOWORLD"   "KZHDFQYHXT")
                 ((0 1 2) "AAA" ("AB" "CD" "EF") "HELLOWORLD" "IKACBBMTBF")))
        (all-pass t))
    (loop for test in tests for i from 1 do
      (destructuring-bind (rotors key plugs plain expected) test
        (let* ((e (make-enigma-obj rotors key plugs))
               (cipher (encrypt e plain))
               (ok (string= cipher expected))
               (status (if ok "PASS" "FAIL")))
          (format t "  Test ~d: ~20a -> ~15a [~a]~%" i plain cipher status)
          (unless ok
            (format t "          Expected ~a, got ~a~%" expected cipher)
            (setf all-pass nil)))))
    (format t "~%  ~a~%" (if all-pass "ALL 6 TESTS PASSED" "SOME TESTS FAILED"))))

(main)
