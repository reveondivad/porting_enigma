;; Enigma Machine - Fennel Implementation (Lisp on Lua)
;; Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
;; PeopleTec Inc. - Guinness World Record Attempt 2026

(local fwd ["EKMFLGDQVZNTOWYHXUSPAIBRCJ"
             "AJDKSIRUXBLHWTMCQGZNPYFVOE"
             "BDFHJLCPRTXVZNYEIWGAKMUSQO"])
(local bwd ["UWYGADFPVZBECKMTHXSLRINQOJ"
             "AJPCZWRLFBDKOTYUQGENHXMIVS"
             "TAGBPCSDQEUFVNZHYIXJWLRKOM"])
(local notch-pos [16 4 21])
(local reflector "YRUHQSLDPXNGOKMIEBFZCWVJAT")

(fn mod26 [a] (% (+ (% a 26) 26) 26))
(fn c2i [c] (- (string.byte c) 65))
(fn i2c [i] (string.char (+ i 65)))
(fn char-at [s i] (string.sub s (+ i 1) (+ i 1)))

(fn make-rotor [num win]
  {:fwd (. fwd num)
   :bwd (. bwd num)
   :notch (. notch-pos num)
   :offset (c2i win)})

(fn fwd-pass [r idx]
  (let [contact (mod26 (+ idx r.offset))]
    (mod26 (- (c2i (char-at r.fwd contact)) r.offset))))

(fn bwd-pass [r idx]
  (let [contact (mod26 (+ idx r.offset))]
    (mod26 (- (c2i (char-at r.bwd contact)) r.offset))))

(fn step-rotor [r]
  (set r.offset (% (+ r.offset 1) 26)))

(fn at-notch [r] (= r.offset r.notch))

(fn make-enigma [rotors key plugboard]
  (let [plug []]
    (for [i 0 25] (tset plug i i))
    (each [_ pair (ipairs (or plugboard []))]
      (let [a (c2i (string.sub pair 1 1))
            b (c2i (string.sub pair 2 2))]
        (tset plug a b)
        (tset plug b a)))
    {:left (make-rotor (. rotors 1) (string.sub key 1 1))
     :middle (make-rotor (. rotors 2) (string.sub key 2 2))
     :right (make-rotor (. rotors 3) (string.sub key 3 3))
     :plug plug}))

(fn step-rotors [e]
  (if (at-notch e.middle)
    (do (step-rotor e.middle) (step-rotor e.left))
    (when (at-notch e.right)
      (step-rotor e.middle)))
  (step-rotor e.right))

(fn press-key [e c]
  (step-rotors e)
  (var idx (c2i c))
  (set idx (. e.plug idx))
  (set idx (fwd-pass e.right idx))
  (set idx (fwd-pass e.middle idx))
  (set idx (fwd-pass e.left idx))
  (set idx (c2i (char-at reflector idx)))
  (set idx (bwd-pass e.left idx))
  (set idx (bwd-pass e.middle idx))
  (set idx (bwd-pass e.right idx))
  (set idx (. e.plug idx))
  (i2c idx))

(fn encrypt [e text]
  (var result "")
  (let [upper (string.upper text)]
    (for [i 1 (length upper)]
      (let [c (string.sub upper i i)]
        (when (and (>= (string.byte c) 65) (<= (string.byte c) 90))
          (set result (.. result (press-key e c)))))))
  result)

;; Test harness
(print "Enigma Machine - Fennel Implementation")
(print "=======================================")

(local tests [
  [[1 2 3] "AAA" []                "AAAAA"        "BDZGO"]
  [[1 2 3] "AAA" []                "HELLOWORLD"   "ILBDAAMTAZ"]
  [[1 2 3] "AAA" []                "ATTACKATDAWN" "BZHGNOCRRTCM"]
  [[1 2 3] "MCK" []                "HELLOWORLD"   "DLTBBQVPQV"]
  [[3 1 2] "AAA" []                "HELLOWORLD"   "KZHDFQYHXT"]
  [[1 2 3] "AAA" ["AB" "CD" "EF"] "HELLOWORLD"   "IKACBBMTBF"]])

(var all-pass true)
(each [i t (ipairs tests)]
  (let [e (make-enigma (. t 1) (. t 2) (. t 3))
        cipher (encrypt e (. t 4))
        expected (. t 5)
        ok (= cipher expected)
        status (if ok "PASS" "FAIL")]
    (print (string.format "  Test %d: %s -> %s [%s]" i (. t 4) cipher status))
    (when (not ok)
      (print (string.format "          Expected %s" expected))
      (set all-pass false))))

(print (if all-pass "\n  ALL 6 TESTS PASSED" "\n  SOME TESTS FAILED"))
