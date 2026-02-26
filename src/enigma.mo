// Enigma Cipher - Modelica
// Equation-based modeling language for physical systems
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

model Enigma
  constant Integer fwdI[26] = {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9};
  constant Integer fwdII[26] = {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4};
  constant Integer fwdIII[26] = {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14};
  constant Integer bwdI[26] = {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9};
  constant Integer bwdII[26] = {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18};
  constant Integer bwdIII[26] = {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12};
  constant Integer reflector[26] = {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19};
  constant Integer notches[3] = {16, 4, 21};

  function mod26
    input Integer n;
    output Integer r;
  algorithm
    r := mod(mod(n, 26) + 26, 26);
  end mod26;

  function getFwd
    input Integer rotor;
    input Integer idx;
    output Integer out;
  algorithm
    if rotor == 0 then
      out := fwdI[idx + 1];
    elseif rotor == 1 then
      out := fwdII[idx + 1];
    else
      out := fwdIII[idx + 1];
    end if;
  end getFwd;

  function getBwd
    input Integer rotor;
    input Integer idx;
    output Integer out;
  algorithm
    if rotor == 0 then
      out := bwdI[idx + 1];
    elseif rotor == 1 then
      out := bwdII[idx + 1];
    else
      out := bwdIII[idx + 1];
    end if;
  end getBwd;

  function passFwd
    input Integer rotor;
    input Integer offset;
    input Integer ch;
    output Integer out;
  protected
    Integer inp;
    Integer raw;
  algorithm
    inp := mod26(ch + offset);
    raw := getFwd(rotor, inp);
    out := mod26(raw - offset);
  end passFwd;

  function passBwd
    input Integer rotor;
    input Integer offset;
    input Integer ch;
    output Integer out;
  protected
    Integer inp;
    Integer raw;
  algorithm
    inp := mod26(ch + offset);
    raw := getBwd(rotor, inp);
    out := mod26(raw - offset);
  end passBwd;

  function encrypt
    input Integer r0; input Integer r1; input Integer r2;
    input Integer k0; input Integer k1; input Integer k2;
    input Integer msg[:];
    output Integer result[size(msg, 1)];
  protected
    Integer o0, o1, o2, n1, n2;
    Integer c;
    Boolean mid, atn;
    Integer pb[26];
  algorithm
    o0 := k0; o1 := k1; o2 := k2;
    n1 := notches[r1 + 1]; n2 := notches[r2 + 1];
    for i in 1:26 loop pb[i] := i - 1; end for;

    for i in 1:size(msg, 1) loop
      mid := o1 == n1;
      atn := o2 == n2;
      o2 := mod26(o2 + 1);
      if atn or mid then o1 := mod26(o1 + 1); end if;
      if mid then o0 := mod26(o0 + 1); end if;
      c := pb[msg[i] + 1];
      c := passFwd(r2, o2, c);
      c := passFwd(r1, o1, c);
      c := passFwd(r0, o0, c);
      c := reflector[c + 1];
      c := passBwd(r0, o0, c);
      c := passBwd(r1, o1, c);
      c := passBwd(r2, o2, c);
      c := pb[c + 1];
      result[i] := c;
    end for;
  end encrypt;

equation
  // Modelica is equation-based; tests run in algorithm section
  when initial() then
    Modelica.Utilities.Streams.print("Enigma Cipher - Modelica");
  end when;

  annotation(experiment(StopTime=0));
end Enigma;
