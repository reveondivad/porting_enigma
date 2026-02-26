# Enigma Cipher - PicoLisp
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

(setq *FwdI (4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9))
(setq *FwdII (0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4))
(setq *FwdIII (1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14))
(setq *BwdI (20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9))
(setq *BwdII (0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18))
(setq *BwdIII (19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12))
(setq *Ref (24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19))
(setq *Notch (16 4 21))

(de mod26 (N)
   (% (+ (% N 26) 26) 26) )

(de getFwd (R I)
   (nth (if (= R 0) *FwdI (if (= R 1) *FwdII *FwdIII)) (inc I)) )

(de getBwd (R I)
   (nth (if (= R 0) *BwdI (if (= R 1) *BwdII *BwdIII)) (inc I)) )

(de passFwd (Rotor Offset Ch)
   (let Inp (mod26 (+ Ch Offset))
      (mod26 (- (car (getFwd Rotor Inp)) Offset)) ) )

(de passBwd (Rotor Offset Ch)
   (let Inp (mod26 (+ Ch Offset))
      (mod26 (- (car (getBwd Rotor Inp)) Offset)) ) )

(de initEnigma (R1 R2 R3 K1 K2 K3)
   (setq *R0 (dec R1) *R1 (dec R2) *R2 (dec R3))
   (setq *O0 K1 *O1 K2 *O2 K3)
   (setq *N0 (car (nth *Notch (inc *R0)))
          *N1 (car (nth *Notch (inc *R1)))
          *N2 (car (nth *Notch (inc *R2))) )
   (setq *Pb (range 0 25)) )

(de stepRotors ()
   (cond
      ((= *O1 *N1)
         (setq *O1 (mod26 (inc *O1)))
         (setq *O0 (mod26 (inc *O0))) )
      ((= *O2 *N2)
         (setq *O1 (mod26 (inc *O1))) ) )
   (setq *O2 (mod26 (inc *O2))) )

(de pressKey (Ch)
   (stepRotors)
   (let C (car (nth *Pb (inc Ch)))
      (setq C (passFwd *R2 *O2 C))
      (setq C (passFwd *R1 *O1 C))
      (setq C (passFwd *R0 *O0 C))
      (setq C (car (nth *Ref (inc C))))
      (setq C (passBwd *R0 *O0 C))
      (setq C (passBwd *R1 *O1 C))
      (setq C (passBwd *R2 *O2 C))
      (car (nth *Pb (inc C))) ) )

(de encrypt (Text)
   (pack
      (mapcar
         '((C) (char (+ 65 (pressKey (- (char C) 65)))))
         (chop Text) ) ) )

(prinl "Enigma Cipher - PicoLisp")
(initEnigma 1 2 3 0 0 0)
(prinl "Test 1: " (encrypt "AAAAA"))
(initEnigma 1 2 3 0 0 0)
(prinl "Test 2: " (encrypt "HELLOWORLD"))
(initEnigma 1 2 3 0 0 0)
(prinl "Test 3: " (encrypt "ATTACKATDAWN"))
(initEnigma 1 2 3 12 2 10)
(prinl "Test 4: " (encrypt "HELLOWORLD"))
(initEnigma 3 1 2 0 0 0)
(prinl "Test 5: " (encrypt "HELLOWORLD"))
