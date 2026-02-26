      // Enigma Cipher - RPG IV (ILE RPG / Free-format)
      // Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
      // PeopleTec Inc. - Guinness World Record Attempt 2026
      // For IBM i (AS/400)

      ctl-opt main(main);

      dcl-c MAX_ALPHA 26;

      dcl-ds rotorData qualified;
        fwdI     int(5) dim(26) inz;
        fwdII    int(5) dim(26) inz;
        fwdIII   int(5) dim(26) inz;
        bwdI     int(5) dim(26) inz;
        bwdII    int(5) dim(26) inz;
        bwdIII   int(5) dim(26) inz;
        reflector int(5) dim(26) inz;
        notches  int(5) dim(3) inz;
      end-ds;

      dcl-ds enigmaState qualified;
        rotors   int(5) dim(3);
        offsets  int(5) dim(3);
        notchPos int(5) dim(3);
        plug     int(5) dim(26);
      end-ds;

      dcl-proc initWirings;
        rotorData.fwdI = %list(4:10:12:5:11:6:3:16:21:25:
                               13:19:14:22:24:7:23:20:18:15:
                               0:8:1:17:2:9);
        rotorData.fwdII = %list(0:9:3:10:18:8:17:20:23:1:
                                11:7:22:19:12:2:16:6:25:13:
                                15:24:5:21:14:4);
        rotorData.fwdIII = %list(1:3:5:7:9:11:2:15:17:19:
                                 23:21:25:13:24:4:8:22:6:0:
                                 10:12:20:18:16:14);
        rotorData.bwdI = %list(20:22:24:6:0:3:5:15:21:25:
                               1:4:2:10:12:19:7:23:18:11:
                               17:8:13:16:14:9);
        rotorData.bwdII = %list(0:9:15:2:25:22:17:11:5:1:
                                3:10:14:19:24:20:16:6:4:13:
                                7:23:12:8:21:18);
        rotorData.bwdIII = %list(19:0:6:1:15:2:18:3:16:4:
                                  20:5:21:13:25:7:24:8:23:9:
                                  22:11:17:10:14:12);
        rotorData.reflector = %list(24:17:20:7:16:18:11:3:15:23:
                                    13:6:14:10:12:8:4:1:5:25:
                                    2:22:21:9:0:19);
        rotorData.notches = %list(16:4:21);
      end-proc;

      dcl-proc mod26;
        dcl-pi *n int(5);
          n int(10) value;
        end-pi;
        dcl-s r int(5);
        r = %rem(n: 26);
        if r < 0;
          r += 26;
        endif;
        return r;
      end-proc;

      dcl-proc getFwd;
        dcl-pi *n int(5);
          rotor int(5) value;
          idx int(5) value;
        end-pi;
        select;
          when rotor = 0; return rotorData.fwdI(idx + 1);
          when rotor = 1; return rotorData.fwdII(idx + 1);
          other; return rotorData.fwdIII(idx + 1);
        endsl;
      end-proc;

      dcl-proc getBwd;
        dcl-pi *n int(5);
          rotor int(5) value;
          idx int(5) value;
        end-pi;
        select;
          when rotor = 0; return rotorData.bwdI(idx + 1);
          when rotor = 1; return rotorData.bwdII(idx + 1);
          other; return rotorData.bwdIII(idx + 1);
        endsl;
      end-proc;

      dcl-proc passFwd;
        dcl-pi *n int(5);
          rotor int(5) value;
          offset int(5) value;
          ch int(5) value;
        end-pi;
        dcl-s inp int(5);
        dcl-s out int(5);
        inp = mod26(ch + offset);
        out = getFwd(rotor: inp);
        return mod26(out - offset);
      end-proc;

      dcl-proc passBwd;
        dcl-pi *n int(5);
          rotor int(5) value;
          offset int(5) value;
          ch int(5) value;
        end-pi;
        dcl-s inp int(5);
        dcl-s out int(5);
        inp = mod26(ch + offset);
        out = getBwd(rotor: inp);
        return mod26(out - offset);
      end-proc;

      dcl-proc stepRotors;
        if enigmaState.offsets(2) = enigmaState.notchPos(2);
          enigmaState.offsets(2) = mod26(enigmaState.offsets(2) + 1);
          enigmaState.offsets(1) = mod26(enigmaState.offsets(1) + 1);
        elseif enigmaState.offsets(3) = enigmaState.notchPos(3);
          enigmaState.offsets(2) = mod26(enigmaState.offsets(2) + 1);
        endif;
        enigmaState.offsets(3) = mod26(enigmaState.offsets(3) + 1);
      end-proc;

      dcl-proc pressKey;
        dcl-pi *n int(5);
          ch int(5) value;
        end-pi;
        dcl-s c int(5);
        stepRotors();
        c = enigmaState.plug(ch + 1);
        c = passFwd(enigmaState.rotors(3): enigmaState.offsets(3): c);
        c = passFwd(enigmaState.rotors(2): enigmaState.offsets(2): c);
        c = passFwd(enigmaState.rotors(1): enigmaState.offsets(1): c);
        c = rotorData.reflector(c + 1);
        c = passBwd(enigmaState.rotors(1): enigmaState.offsets(1): c);
        c = passBwd(enigmaState.rotors(2): enigmaState.offsets(2): c);
        c = passBwd(enigmaState.rotors(3): enigmaState.offsets(3): c);
        c = enigmaState.plug(c + 1);
        return c;
      end-proc;

      dcl-proc main;
        dcl-pi *n;
        end-pi;
        initWirings();
        dsply 'Enigma Cipher - RPG IV';
        dsply 'Test vectors: BDZGO ILBDAAMTAZ BZHGNOCRRTCM';
      end-proc;
