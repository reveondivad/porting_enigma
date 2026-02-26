       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENIGMA.
       AUTHOR. PEOPLETEC-INC.
      * Enigma Machine - COBOL Implementation
      * Wehrmacht Enigma I (3-rotor, Reflector B, plugboard)
      * Guinness World Record Attempt 2026

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 FWD-I    PIC X(26) VALUE "EKMFLGDQVZNTOWYHXUSPAIBRCJ".
       01 FWD-II   PIC X(26) VALUE "AJDKSIRUXBLHWTMCQGZNPYFVOE".
       01 FWD-III  PIC X(26) VALUE "BDFHJLCPRTXVZNYEIWGAKMUSQO".
       01 BWD-I    PIC X(26) VALUE "UWYGADFPVZBECKMTHXSLRINQOJ".
       01 BWD-II   PIC X(26) VALUE "AJPCZWRLFBDKOTYUQGENHXMIVS".
       01 BWD-III  PIC X(26) VALUE "TAGBPCSDQEUFVNZHYIXJWLRKOM".
       01 REFL     PIC X(26) VALUE "YRUHQSLDPXNGOKMIEBFZCWVJAT".

       01 NOTCH-I   PIC 9(2) VALUE 16.
       01 NOTCH-II  PIC 9(2) VALUE 04.
       01 NOTCH-III PIC 9(2) VALUE 21.

       01 LEFT-FWD   PIC X(26).
       01 LEFT-BWD   PIC X(26).
       01 LEFT-NOTCH  PIC 9(2).
       01 LEFT-OFF   PIC 9(2).

       01 MID-FWD   PIC X(26).
       01 MID-BWD   PIC X(26).
       01 MID-NOTCH  PIC 9(2).
       01 MID-OFF   PIC 9(2).

       01 RIGHT-FWD   PIC X(26).
       01 RIGHT-BWD   PIC X(26).
       01 RIGHT-NOTCH  PIC 9(2).
       01 RIGHT-OFF   PIC 9(2).

       01 PLUG-TABLE.
          05 PLUG-ENTRY PIC 9(2) OCCURS 26 TIMES.

       01 WS-IDX     PIC 9(2).
       01 WS-CONTACT PIC 9(2).
       01 WS-OUT     PIC 9(2).
       01 WS-TEMP    PIC S9(4).
       01 WS-CHAR    PIC X(1).
       01 WS-RESULT  PIC X(100).
       01 WS-RESLEN  PIC 9(3) VALUE 0.
       01 WS-I       PIC 9(3).
       01 WS-INCHAR  PIC X(1).
       01 WS-INNUM   PIC 9(2).

       01 TEST-PLAIN  PIC X(20).
       01 TEST-EXPECT PIC X(20).
       01 TEST-CIPHER PIC X(20).
       01 TEST-NUM    PIC 9(1).
       01 ALL-PASS    PIC 9(1) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enigma Machine - COBOL Implementation"
           DISPLAY "======================================"

      * Test 1: I-II-III AAA no-plug AAAAA -> BDZGO
           PERFORM INIT-ROTOR-1-2-3-AAA
           MOVE "AAAAA" TO TEST-PLAIN
           MOVE "BDZGO" TO TEST-EXPECT
           MOVE 1 TO TEST-NUM
           PERFORM ENCRYPT-AND-CHECK

      * Test 2: I-II-III AAA no-plug HELLOWORLD -> ILBDAAMTAZ
           PERFORM INIT-ROTOR-1-2-3-AAA
           MOVE "HELLOWORLD" TO TEST-PLAIN
           MOVE "ILBDAAMTAZ" TO TEST-EXPECT
           MOVE 2 TO TEST-NUM
           PERFORM ENCRYPT-AND-CHECK

      * Test 3: I-II-III AAA no-plug ATTACKATDAWN -> BZHGNOCRRTCM
           PERFORM INIT-ROTOR-1-2-3-AAA
           MOVE "ATTACKATDAWN" TO TEST-PLAIN
           MOVE "BZHGNOCRRTCM" TO TEST-EXPECT
           MOVE 3 TO TEST-NUM
           PERFORM ENCRYPT-AND-CHECK

      * Test 4: I-II-III MCK no-plug HELLOWORLD -> DLTBBQVPQV
           PERFORM INIT-ROTOR-1-2-3-MCK
           MOVE "HELLOWORLD" TO TEST-PLAIN
           MOVE "DLTBBQVPQV" TO TEST-EXPECT
           MOVE 4 TO TEST-NUM
           PERFORM ENCRYPT-AND-CHECK

      * Test 5: III-I-II AAA no-plug HELLOWORLD -> KZHDFQYHXT
           PERFORM INIT-ROTOR-3-1-2-AAA
           MOVE "HELLOWORLD" TO TEST-PLAIN
           MOVE "KZHDFQYHXT" TO TEST-EXPECT
           MOVE 5 TO TEST-NUM
           PERFORM ENCRYPT-AND-CHECK

      * Test 6: I-II-III AAA AB-CD-EF HELLOWORLD -> IKACBBMTBF
           PERFORM INIT-ROTOR-1-2-3-AAA-PLUGS
           MOVE "HELLOWORLD" TO TEST-PLAIN
           MOVE "IKACBBMTBF" TO TEST-EXPECT
           MOVE 6 TO TEST-NUM
           PERFORM ENCRYPT-AND-CHECK

           IF ALL-PASS = 1
               DISPLAY "  ALL 6 TESTS PASSED"
           ELSE
               DISPLAY "  SOME TESTS FAILED"
           END-IF
           STOP RUN.

       INIT-ROTOR-1-2-3-AAA.
           MOVE FWD-I TO LEFT-FWD
           MOVE BWD-I TO LEFT-BWD
           MOVE NOTCH-I TO LEFT-NOTCH
           MOVE 0 TO LEFT-OFF
           MOVE FWD-II TO MID-FWD
           MOVE BWD-II TO MID-BWD
           MOVE NOTCH-II TO MID-NOTCH
           MOVE 0 TO MID-OFF
           MOVE FWD-III TO RIGHT-FWD
           MOVE BWD-III TO RIGHT-BWD
           MOVE NOTCH-III TO RIGHT-NOTCH
           MOVE 0 TO RIGHT-OFF
           PERFORM INIT-PLUG-IDENTITY.

       INIT-ROTOR-1-2-3-MCK.
           PERFORM INIT-ROTOR-1-2-3-AAA
           MOVE 12 TO LEFT-OFF
           MOVE 2 TO MID-OFF
           MOVE 10 TO RIGHT-OFF.

       INIT-ROTOR-3-1-2-AAA.
           MOVE FWD-III TO LEFT-FWD
           MOVE BWD-III TO LEFT-BWD
           MOVE NOTCH-III TO LEFT-NOTCH
           MOVE 0 TO LEFT-OFF
           MOVE FWD-I TO MID-FWD
           MOVE BWD-I TO MID-BWD
           MOVE NOTCH-I TO MID-NOTCH
           MOVE 0 TO MID-OFF
           MOVE FWD-II TO RIGHT-FWD
           MOVE BWD-II TO RIGHT-BWD
           MOVE NOTCH-II TO RIGHT-NOTCH
           MOVE 0 TO RIGHT-OFF
           PERFORM INIT-PLUG-IDENTITY.

       INIT-ROTOR-1-2-3-AAA-PLUGS.
           PERFORM INIT-ROTOR-1-2-3-AAA
           MOVE 1 TO PLUG-ENTRY(1)
           MOVE 0 TO PLUG-ENTRY(2)
           MOVE 3 TO PLUG-ENTRY(3)
           MOVE 2 TO PLUG-ENTRY(4)
           MOVE 5 TO PLUG-ENTRY(5)
           MOVE 4 TO PLUG-ENTRY(6).

       INIT-PLUG-IDENTITY.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 26
               COMPUTE PLUG-ENTRY(WS-I) = WS-I - 1
           END-PERFORM.

       STEP-ROTORS.
           IF MID-OFF = MID-NOTCH
               ADD 1 TO MID-OFF
               IF MID-OFF > 25
                   MOVE 0 TO MID-OFF
               END-IF
               ADD 1 TO LEFT-OFF
               IF LEFT-OFF > 25
                   MOVE 0 TO LEFT-OFF
               END-IF
           ELSE
               IF RIGHT-OFF = RIGHT-NOTCH
                   ADD 1 TO MID-OFF
                   IF MID-OFF > 25
                       MOVE 0 TO MID-OFF
                   END-IF
               END-IF
           END-IF
           ADD 1 TO RIGHT-OFF
           IF RIGHT-OFF > 25
               MOVE 0 TO RIGHT-OFF
           END-IF.

       FWD-PASS-RIGHT.
           COMPUTE WS-TEMP = WS-IDX + RIGHT-OFF
           COMPUTE WS-CONTACT =
               FUNCTION MOD(WS-TEMP, 26)
           MOVE RIGHT-FWD(WS-CONTACT + 1:1) TO WS-CHAR
           COMPUTE WS-TEMP =
               FUNCTION ORD(WS-CHAR) - FUNCTION ORD("A")
               - RIGHT-OFF
           PERFORM MODULO-26.

       FWD-PASS-MID.
           COMPUTE WS-TEMP = WS-IDX + MID-OFF
           COMPUTE WS-CONTACT =
               FUNCTION MOD(WS-TEMP, 26)
           MOVE MID-FWD(WS-CONTACT + 1:1) TO WS-CHAR
           COMPUTE WS-TEMP =
               FUNCTION ORD(WS-CHAR) - FUNCTION ORD("A")
               - MID-OFF
           PERFORM MODULO-26.

       FWD-PASS-LEFT.
           COMPUTE WS-TEMP = WS-IDX + LEFT-OFF
           COMPUTE WS-CONTACT =
               FUNCTION MOD(WS-TEMP, 26)
           MOVE LEFT-FWD(WS-CONTACT + 1:1) TO WS-CHAR
           COMPUTE WS-TEMP =
               FUNCTION ORD(WS-CHAR) - FUNCTION ORD("A")
               - LEFT-OFF
           PERFORM MODULO-26.

       BWD-PASS-LEFT.
           COMPUTE WS-TEMP = WS-IDX + LEFT-OFF
           COMPUTE WS-CONTACT =
               FUNCTION MOD(WS-TEMP, 26)
           MOVE LEFT-BWD(WS-CONTACT + 1:1) TO WS-CHAR
           COMPUTE WS-TEMP =
               FUNCTION ORD(WS-CHAR) - FUNCTION ORD("A")
               - LEFT-OFF
           PERFORM MODULO-26.

       BWD-PASS-MID.
           COMPUTE WS-TEMP = WS-IDX + MID-OFF
           COMPUTE WS-CONTACT =
               FUNCTION MOD(WS-TEMP, 26)
           MOVE MID-BWD(WS-CONTACT + 1:1) TO WS-CHAR
           COMPUTE WS-TEMP =
               FUNCTION ORD(WS-CHAR) - FUNCTION ORD("A")
               - MID-OFF
           PERFORM MODULO-26.

       BWD-PASS-RIGHT.
           COMPUTE WS-TEMP = WS-IDX + RIGHT-OFF
           COMPUTE WS-CONTACT =
               FUNCTION MOD(WS-TEMP, 26)
           MOVE RIGHT-BWD(WS-CONTACT + 1:1) TO WS-CHAR
           COMPUTE WS-TEMP =
               FUNCTION ORD(WS-CHAR) - FUNCTION ORD("A")
               - RIGHT-OFF
           PERFORM MODULO-26.

       MODULO-26.
           COMPUTE WS-IDX = FUNCTION MOD(WS-TEMP + 260, 26).

       REFLECT.
           MOVE REFL(WS-IDX + 1:1) TO WS-CHAR
           COMPUTE WS-IDX =
               FUNCTION ORD(WS-CHAR) - FUNCTION ORD("A").

       PRESS-KEY.
           PERFORM STEP-ROTORS
           COMPUTE WS-IDX =
               FUNCTION ORD(WS-INCHAR) - FUNCTION ORD("A")
           MOVE PLUG-ENTRY(WS-IDX + 1) TO WS-IDX
           PERFORM FWD-PASS-RIGHT
           PERFORM FWD-PASS-MID
           PERFORM FWD-PASS-LEFT
           PERFORM REFLECT
           PERFORM BWD-PASS-LEFT
           PERFORM BWD-PASS-MID
           PERFORM BWD-PASS-RIGHT
           MOVE PLUG-ENTRY(WS-IDX + 1) TO WS-IDX
           COMPUTE WS-INNUM = WS-IDX + FUNCTION ORD("A")
           MOVE FUNCTION CHAR(WS-INNUM) TO WS-INCHAR.

       ENCRYPT-AND-CHECK.
           MOVE SPACES TO WS-RESULT
           MOVE 0 TO WS-RESLEN
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > FUNCTION LENGTH(
                   FUNCTION TRIM(TEST-PLAIN))
               MOVE TEST-PLAIN(WS-I:1) TO WS-INCHAR
               IF WS-INCHAR >= "A" AND WS-INCHAR <= "Z"
                   PERFORM PRESS-KEY
                   ADD 1 TO WS-RESLEN
                   MOVE WS-INCHAR TO
                       WS-RESULT(WS-RESLEN:1)
               END-IF
           END-PERFORM
           MOVE WS-RESULT TO TEST-CIPHER
           IF FUNCTION TRIM(TEST-CIPHER) =
              FUNCTION TRIM(TEST-EXPECT)
               DISPLAY "  Test " TEST-NUM ": "
                   FUNCTION TRIM(TEST-PLAIN) " -> "
                   FUNCTION TRIM(TEST-CIPHER) " [PASS]"
           ELSE
               DISPLAY "  Test " TEST-NUM ": "
                   FUNCTION TRIM(TEST-PLAIN) " -> "
                   FUNCTION TRIM(TEST-CIPHER) " [FAIL]"
               MOVE 0 TO ALL-PASS
           END-IF.
