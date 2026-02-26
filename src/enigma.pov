// Enigma cipher in POV-Ray Scene Description Language
#declare RF1 = array[26] {4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9}
#declare RF2 = array[26] {0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4}
#declare RF3 = array[26] {1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14}
#declare RB1 = array[26] {20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9}
#declare RB2 = array[26] {0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18}
#declare RB3 = array[26] {19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12}
#declare REF = array[26] {24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19}
#declare NOTCH = array[3] {16, 4, 21}

#macro Mod26(N)
  #local M = mod(N, 26);
  #if (M < 0)
    #local M = M + 26;
  #end
  M
#end

#macro RotorPass(Wiring, C, Pos)
  Mod26(Wiring[Mod26(C + Pos)] - Pos)
#end

// Encrypt character 7 (H) with rotor pos [0,0,1]
#declare C = 7;
#declare C = RotorPass(RF3, C, 1);
#declare C = RotorPass(RF2, C, 0);
#declare C = RotorPass(RF1, C, 0);
#declare C = REF[C];
#debug concat("Enigma POV-Ray: H -> ", str(C, 0, 0), "\n")

// Visual representation of Enigma rotors
camera { location <0, 5, -10> look_at <0, 0, 0> }
light_source { <10, 10, -10> color rgb 1 }

#declare I = 0;
#while (I < 26)
  sphere {
    <cos(I * 2 * pi / 26) * 3, 0, sin(I * 2 * pi / 26) * 3>, 0.2
    pigment { color rgb <RF1[I]/25, 0.3, 0.7> }
  }
  #declare I = I + 1;
#end
