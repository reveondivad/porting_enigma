;;;; Enigma cipher in Common Lisp
(defparameter *rotor-fwd*
  #(#(4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9)
    #(0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4)
    #(1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14)))

(defparameter *rotor-bwd*
  #(#(20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9)
    #(0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18)
    #(19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12)))

(defparameter *reflector* #(24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19))
(defparameter *notches* #(16 4 21))

(defun mod26 (n) (mod (+ (mod n 26) 26) 26))

(defun rotor-pass (wiring c pos)
  (mod26 (- (aref wiring (mod26 (+ c pos))) pos)))

(defun enigma (text)
  (let ((pos (vector 0 0 0))
        (result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for ch across (string-upcase text)
          for c = (- (char-code ch) 65)
          when (and (>= c 0) (< c 26))
          do (let ((mid (= (aref pos 1) (aref *notches* 1))))
               (when (= (aref pos 2) (aref *notches* 2))
                 (setf (aref pos 2) (mod26 (1+ (aref pos 2)))))
               (when (or mid (= (aref pos 2) (aref *notches* 2)))
                 (setf (aref pos 1) (mod26 (1+ (aref pos 1)))))
               (setf (aref pos 2) (mod26 (1+ (aref pos 2)))))
             (loop for i from 2 downto 0
                   do (setf c (rotor-pass (aref *rotor-fwd* i) c (aref pos i))))
             (setf c (aref *reflector* c))
             (loop for i from 0 to 2
                   do (setf c (rotor-pass (aref *rotor-bwd* i) c (aref pos i))))
             (vector-push-extend (code-char (+ c 65)) result))
    result))

(format t "~a~%" (enigma "HELLOWORLD"))
