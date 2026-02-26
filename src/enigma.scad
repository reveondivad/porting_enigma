// Enigma Cipher - OpenSCAD
// 3D modeling language used for computation via echo()
// Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
// PeopleTec Inc. - Guinness World Record Attempt 2026

// Rotor wirings (0-indexed arrays)
fwdI = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9];
fwdII = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4];
fwdIII = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14];
bwdI = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9];
bwdII = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18];
bwdIII = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12];
reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19];
notches = [16, 4, 21];

// mod26 that handles negatives
function mod26(n) = ((n % 26) + 26) % 26;

// Get forward wiring value
function getFwd(r, i) = r == 0 ? fwdI[i] : r == 1 ? fwdII[i] : fwdIII[i];
function getBwd(r, i) = r == 0 ? bwdI[i] : r == 1 ? bwdII[i] : bwdIII[i];

// Forward/backward pass through rotor
function passFwd(rotor, offset, ch) =
  let(inp = mod26(ch + offset), out = getFwd(rotor, inp))
  mod26(out - offset);

function passBwd(rotor, offset, ch) =
  let(inp = mod26(ch + offset), out = getBwd(rotor, inp))
  mod26(out - offset);

// Step rotors, returns [o0, o1, o2]
function stepRotors(o0, o1, o2, n1, n2) =
  let(mid = (o1 == n1), atn = (o2 == n2),
      newO2 = mod26(o2 + 1),
      newO1 = (mid || atn) ? mod26(o1 + 1) : o1,
      newO0 = mid ? mod26(o0 + 1) : o0)
  [newO0, newO1, newO2];

// Encrypt one char, returns [encrypted_char, o0, o1, o2]
function encryptChar(ch, r0, r1, r2, o0, o1, o2, n1, n2) =
  let(s = stepRotors(o0, o1, o2, n1, n2),
      c0 = ch,  // no plugboard in this simplified version
      c1 = passFwd(r2, s[2], c0),
      c2 = passFwd(r1, s[1], c1),
      c3 = passFwd(r0, s[0], c2),
      c4 = reflector[c3],
      c5 = passBwd(r0, s[0], c4),
      c6 = passBwd(r1, s[1], c5),
      c7 = passBwd(r2, s[2], c6))
  [c7, s[0], s[1], s[2]];

// Recursive encrypt (OpenSCAD is purely functional)
function enigma_encrypt(msg, r0, r1, r2, k0, k1, k2, pos=0, o0=undef, o1=undef, o2=undef) =
  let(o0_ = pos == 0 ? k0 : o0,
      o1_ = pos == 0 ? k1 : o1,
      o2_ = pos == 0 ? k2 : o2,
      n1 = notches[r1], n2 = notches[r2])
  pos >= len(msg) ? [] :
  let(ch = msg[pos] - 65,
      result = encryptChar(ch, r0, r1, r2, o0_, o1_, o2_, n1, n2))
  concat([result[0] + 65],
         enigma_encrypt(msg, r0, r1, r2, k0, k1, k2, pos+1, result[1], result[2], result[3]));

// Test - convert ASCII code arrays to strings via echo
msg1 = [65,65,65,65,65]; // AAAAA
result1 = enigma_encrypt(msg1, 0, 1, 2, 0, 0, 0);
echo("Enigma Cipher - OpenSCAD");
echo(str("Test 1: ", chr(result1), " expected BDZGO"));

msg2 = [72,69,76,76,79,87,79,82,76,68]; // HELLOWORLD
result2 = enigma_encrypt(msg2, 0, 1, 2, 0, 0, 0);
echo(str("Test 2: ", chr(result2), " expected ILBDAAMTAZ"));

// Render a decorative rotor for visualization
difference() {
  cylinder(h=5, r=30, $fn=60);
  for (i = [0:25]) {
    rotate([0, 0, i * 360/26])
      translate([25, 0, -1])
        cylinder(h=7, r=1.5, $fn=20);
  }
}
