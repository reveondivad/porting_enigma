;; Enigma Cipher - Hy (Lisp embedded in Python)
;; Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
;; PeopleTec Inc. - Guinness World Record Attempt 2026

(setv FWD [[4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9]
           [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4]
           [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14]])

(setv BWD [[20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9]
           [0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18]
           [19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12]])

(setv REF [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19])
(setv NOTCH [16 4 21])

(defn mod26 [n] (% (+ (% n 26) 26) 26))

(defn pass-fwd [rotor offset ch]
  (let [inp (mod26 (+ ch offset))
        out (get (get FWD rotor) inp)]
    (mod26 (- out offset))))

(defn pass-bwd [rotor offset ch]
  (let [inp (mod26 (+ ch offset))
        out (get (get BWD rotor) inp)]
    (mod26 (- out offset))))

(defn encrypt [r0 r1 r2 k0 k1 k2 pairs msg]
  (setv pb (list (range 26)))
  (for [p pairs]
    (setv (get pb (get p 0)) (get p 1))
    (setv (get pb (get p 1)) (get p 0)))
  (setv o0 k0 o1 k1 o2 k2)
  (setv result [])
  (for [char msg]
    (setv ch (- (ord char) 65))
    (setv mid (= o1 (get NOTCH r1)))
    (setv atn (= o2 (get NOTCH r2)))
    (setv o2 (mod26 (+ o2 1)))
    (when (or atn mid) (setv o1 (mod26 (+ o1 1))))
    (when mid (setv o0 (mod26 (+ o0 1))))
    (setv c (get pb ch))
    (setv c (pass-fwd r2 o2 c))
    (setv c (pass-fwd r1 o1 c))
    (setv c (pass-fwd r0 o0 c))
    (setv c (get REF c))
    (setv c (pass-bwd r0 o0 c))
    (setv c (pass-bwd r1 o1 c))
    (setv c (pass-bwd r2 o2 c))
    (setv c (get pb c))
    (.append result (chr (+ c 65))))
  (.join "" result))

(defn run-test [label expected r0 r1 r2 k0 k1 k2 pairs msg]
  (setv actual (encrypt r0 r1 r2 k0 k1 k2 pairs msg))
  (print f"{(if (= actual expected) \"PASS\" \"FAIL\")} {label}: {actual} (expected {expected})"))

(print "Enigma Cipher - Hy")
(run-test "Test 1" "BDZGO" 0 1 2 0 0 0 [] "AAAAA")
(run-test "Test 2" "ILBDAAMTAZ" 0 1 2 0 0 0 [] "HELLOWORLD")
(run-test "Test 3" "BZHGNOCRRTCM" 0 1 2 0 0 0 [] "ATTACKATDAWN")
(run-test "Test 4" "DLTBBQVPQV" 0 1 2 12 2 10 [] "HELLOWORLD")
(run-test "Test 5" "KZHDFQYHXT" 2 0 1 0 0 0 [] "HELLOWORLD")
(run-test "Test 6" "IKACBBMTBF" 0 1 2 0 0 0 [[0 1] [2 3] [4 5]] "HELLOWORLD")
