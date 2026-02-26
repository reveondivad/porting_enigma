# Enigma Cipher - Janet
# Functional Lisp for system scripting

(def fwd-I @[4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9])
(def fwd-II @[0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4])
(def fwd-III @[1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14])

(def bwd-I @[20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9])
(def bwd-II @[0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18])
(def bwd-III @[19 0 6 1 15 2 18 3 16 4 20 9 21 13 25 7 24 8 23 5 22 11 17 12 14 10])

(def notches @[16 4 21])
(def reflector-b @[24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19])

(defn mod26 [x]
  (mod (+ (mod x 26) 26) 26))

(defn get-fwd [r i]
  (case r
    0 (get fwd-I i)
    1 (get fwd-II i)
    2 (get fwd-III i)))

(defn get-bwd [r i]
  (case r
    0 (get bwd-I i)
    1 (get bwd-II i)
    2 (get bwd-III i)))

(defn pass-fwd [rotor offset ch]
  (let [inp (mod26 (+ ch offset))
        out (get-fwd rotor inp)]
    (mod26 (- out offset))))

(defn pass-bwd [rotor offset ch]
  (let [inp (mod26 (+ ch offset))
        out (get-bwd rotor inp)]
    (mod26 (- out offset))))

(defn make-plugboard [pairs]
  (def pb (array/new 26))
  (for i 0 26 (array/push pb i))
  (each p pairs
    (put pb (p 0) (p 1))
    (put pb (p 1) (p 0)))
  pb)

(defn encrypt [r0 r1 r2 k0 k1 k2 pairs msg]
  (def pb (make-plugboard pairs))
  (var o0 k0) (var o1 k1) (var o2 k2)
  (def result @"")
  (each ch msg
    (def c0 (- ch (chr "A")))
    (def mid (= o1 (get notches r1)))
    (def atn (= o2 (get notches r2)))
    (set o2 (mod26 (+ o2 1)))
    (when (or atn mid) (set o1 (mod26 (+ o1 1))))
    (when mid (set o0 (mod26 (+ o0 1))))
    (var c (get pb c0))
    (set c (pass-fwd r2 o2 c))
    (set c (pass-fwd r1 o1 c))
    (set c (pass-fwd r0 o0 c))
    (set c (get reflector-b c))
    (set c (pass-bwd r0 o0 c))
    (set c (pass-bwd r1 o1 c))
    (set c (pass-bwd r2 o2 c))
    (set c (get pb c))
    (buffer/push result (+ c (chr "A"))))
  (string result))

(defn run-test [label expected actual]
  (def status (if (= expected actual) "PASS" "FAIL"))
  (printf "%s %s: %s (expected %s)" status label actual expected))

(print "Enigma Cipher - Janet")
(run-test "Test 1" "BDZGO"
  (encrypt 0 1 2 0 0 0 @[] "AAAAA"))
(run-test "Test 2" "ILBDAAMTAZ"
  (encrypt 0 1 2 0 0 0 @[] "HELLOWORLD"))
(run-test "Test 3" "BZHGNOCRRTCM"
  (encrypt 0 1 2 0 0 0 @[] "ATTACKATDAWN"))
(run-test "Test 4" "DLTBBQVPQV"
  (encrypt 0 1 2 12 2 10 @[] "HELLOWORLD"))
(run-test "Test 5" "KZHDFQYHXT"
  (encrypt 2 0 1 0 0 0 @[] "HELLOWORLD"))
(run-test "Test 6" "IKACBBMTBF"
  (encrypt 0 1 2 0 0 0 @[@[0 1] @[2 3] @[4 5]] "HELLOWORLD"))
