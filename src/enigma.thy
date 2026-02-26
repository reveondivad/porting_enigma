(* Enigma cipher in Isabelle/HOL *)
theory Enigma imports Main begin

definition rotor_fwd_I :: "nat list" where
  "rotor_fwd_I = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]"
definition rotor_fwd_II :: "nat list" where
  "rotor_fwd_II = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]"
definition rotor_fwd_III :: "nat list" where
  "rotor_fwd_III = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]"

definition rotor_bwd_I :: "nat list" where
  "rotor_bwd_I = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]"
definition rotor_bwd_II :: "nat list" where
  "rotor_bwd_II = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]"
definition rotor_bwd_III :: "nat list" where
  "rotor_bwd_III = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]"

definition reflector_B :: "nat list" where
  "reflector_B = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]"

definition notches :: "nat list" where "notches = [16,4,21]"

fun mod26 :: "int \<Rightarrow> nat" where
  "mod26 n = nat ((n mod 26 + 26) mod 26)"

fun rotor_pass :: "nat list \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat" where
  "rotor_pass wiring c pos = mod26 (int (wiring ! mod26 (int c + int pos)) - int pos)"

fun enigma_char :: "nat \<Rightarrow> nat list \<Rightarrow> nat" where
  "enigma_char c pos = (let
    c1 = rotor_pass rotor_fwd_III c (pos!2);
    c2 = rotor_pass rotor_fwd_II c1 (pos!1);
    c3 = rotor_pass rotor_fwd_I c2 (pos!0);
    cr = reflector_B ! c3;
    c4 = rotor_pass rotor_bwd_I cr (pos!0);
    c5 = rotor_pass rotor_bwd_II c4 (pos!1);
    c6 = rotor_pass rotor_bwd_III c5 (pos!2)
  in c6)"

end
