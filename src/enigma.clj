;; Enigma Machine - Clojure Implementation
;; Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
;; PeopleTec Inc. - Guinness World Record Attempt 2026

(ns enigma)

(def fwd-wirings
  {1 "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
   2 "AJDKSIRUXBLHWTMCQGZNPYFVOE"
   3 "BDFHJLCPRTXVZNYEIWGAKMUSQO"})

(def bwd-wirings
  {1 "UWYGADFPVZBECKMTHXSLRINQOJ"
   2 "AJPCZWRLFBDKOTYUQGENHXMIVS"
   3 "TAGBPCSDQEUFVNZHYIXJWLRKOM"})

(def notches {1 16, 2 4, 3 21})
(def reflector "YRUHQSLDPXNGOKMIEBFZCWVJAT")

(defn mod26 [a] (mod (+ (mod a 26) 26) 26))
(defn c2i [c] (- (int c) (int \A)))
(defn i2c [i] (char (+ i (int \A))))

(defn make-rotor [num win]
  {:fwd (fwd-wirings num) :bwd (bwd-wirings num)
   :notch (notches num) :offset (c2i win)})

(defn fwd-pass [{:keys [fwd offset]} idx]
  (let [contact (mod26 (+ idx offset))]
    (mod26 (- (c2i (nth fwd contact)) offset))))

(defn bwd-pass [{:keys [bwd offset]} idx]
  (let [contact (mod26 (+ idx offset))]
    (mod26 (- (c2i (nth bwd contact)) offset))))

(defn step-rotor [r] (update r :offset #(mod (inc %) 26)))

(defn make-plug [pairs]
  (reduce (fn [p pair]
            (let [a (c2i (first pair)) b (c2i (second pair))]
              (assoc p a b b a)))
          (into {} (map (fn [i] [i i]) (range 26)))
          pairs))

(defn step-rotors [{:keys [left middle right] :as state}]
  (let [mid-at-notch (= (:offset middle) (:notch middle))
        right-at-notch (= (:offset right) (:notch right))]
    (cond-> state
      mid-at-notch (update :middle step-rotor)
      mid-at-notch (update :left step-rotor)
      (and (not mid-at-notch) right-at-notch) (update :middle step-rotor)
      true (update :right step-rotor))))

(defn press-key [state ch]
  (let [s (step-rotors state)
        {:keys [left middle right plug]} s
        idx (c2i ch)
        idx (plug idx)
        idx (fwd-pass right idx)
        idx (fwd-pass middle idx)
        idx (fwd-pass left idx)
        idx (c2i (nth reflector idx))
        idx (bwd-pass left idx)
        idx (bwd-pass middle idx)
        idx (bwd-pass right idx)
        idx (plug idx)]
    [s (i2c idx)]))

(defn encrypt [state text]
  (let [chars (filter #(Character/isLetter %) (.toUpperCase text))]
    (loop [s state cs chars result []]
      (if (empty? cs)
        (apply str result)
        (let [[s2 out] (press-key s (first cs))]
          (recur s2 (rest cs) (conj result out)))))))

(defn make-enigma [[r1 r2 r3] key plugboard]
  {:left (make-rotor r1 (nth key 0))
   :middle (make-rotor r2 (nth key 1))
   :right (make-rotor r3 (nth key 2))
   :plug (make-plug (map seq plugboard))})

(defn -main []
  (println "Enigma Machine - Clojure Implementation")
  (println "========================================")
  (let [tests [[[1 2 3] "AAA" []               "AAAAA"        "BDZGO"]
               [[1 2 3] "AAA" []               "HELLOWORLD"   "ILBDAAMTAZ"]
               [[1 2 3] "AAA" []               "ATTACKATDAWN" "BZHGNOCRRTCM"]
               [[1 2 3] "MCK" []               "HELLOWORLD"   "DLTBBQVPQV"]
               [[3 1 2] "AAA" []               "HELLOWORLD"   "KZHDFQYHXT"]
               [[1 2 3] "AAA" ["AB" "CD" "EF"] "HELLOWORLD"   "IKACBBMTBF"]]
        all-pass (atom true)]
    (doseq [[i [rotors key plugs plain expected]] (map-indexed vector tests)]
      (let [m (make-enigma rotors key plugs)
            cipher (encrypt m plain)
            ok (= cipher expected)
            status (if ok "PASS" "FAIL")]
        (printf "  Test %d: %-20s -> %-15s [%s]\n" (inc i) plain cipher status)
        (when (not ok)
          (printf "          Expected %s, got %s\n" expected cipher)
          (reset! all-pass false))))
    (println (if @all-pass "\n  ALL 6 TESTS PASSED" "\n  SOME TESTS FAILED"))))

(-main)
