;; Enigma Cipher - WebAssembly Text Format (WAT)
;; Low-level stack machine, runs in browsers

(module
  ;; Memory for rotor data: 7 arrays of 26 bytes each = 182 bytes
  ;; Layout: fwdI[0..25] fwdII[26..51] fwdIII[52..77]
  ;;         bwdI[78..103] bwdII[104..129] bwdIII[130..155]
  ;;         reflector[156..181] notch[182..184] pb[185..210]
  ;;         offsets[211..213] result[214..]
  (memory (export "memory") 1)

  ;; Initialize rotor data
  (data (i32.const 0)
    ;; fwd I
    "\04\0a\0c\05\0b\06\03\10\15\19\0d\13\0e\16\18\07\17\14\12\0f\00\08\01\11\02\09"
    ;; fwd II
    "\00\09\03\0a\12\08\11\14\17\01\0b\07\16\13\0c\02\10\06\19\0d\0f\18\05\15\0e\04"
    ;; fwd III
    "\01\03\05\07\09\0b\02\0f\11\13\17\15\19\0d\18\04\08\16\06\00\0a\0c\14\12\10\0e"
    ;; bwd I
    "\14\16\18\06\00\03\05\0f\15\19\01\04\02\0a\0c\13\07\17\12\0b\11\08\0d\10\0e\09"
    ;; bwd II
    "\00\09\0f\02\19\16\11\0b\05\01\03\0a\0e\13\18\14\10\06\04\0d\07\17\0c\08\15\12"
    ;; bwd III
    "\13\00\06\01\0f\02\12\03\10\04\14\09\15\0d\19\07\18\08\17\05\16\0b\11\0c\0e\0a"
    ;; reflector B
    "\18\11\14\07\10\12\0b\03\0f\17\0d\06\0e\0a\0c\08\04\01\05\19\02\16\15\09\00\13"
    ;; notches: I=16, II=4, III=21
    "\10\04\15"
  )

  (func $mod26 (param $x i32) (result i32)
    (i32.rem_u
      (i32.add
        (i32.rem_u (local.get $x) (i32.const 26))
        (i32.const 26))
      (i32.const 26)))

  (func $get_fwd (param $rotor i32) (param $idx i32) (result i32)
    (i32.load8_u
      (i32.add
        (i32.mul (local.get $rotor) (i32.const 26))
        (local.get $idx))))

  (func $get_bwd (param $rotor i32) (param $idx i32) (result i32)
    (i32.load8_u
      (i32.add
        (i32.add (i32.const 78)
          (i32.mul (local.get $rotor) (i32.const 26)))
        (local.get $idx))))

  (func $get_reflector (param $idx i32) (result i32)
    (i32.load8_u (i32.add (i32.const 156) (local.get $idx))))

  (func $get_notch (param $rotor i32) (result i32)
    (i32.load8_u (i32.add (i32.const 182) (local.get $rotor))))

  (func $get_pb (param $idx i32) (result i32)
    (i32.load8_u (i32.add (i32.const 185) (local.get $idx))))

  (func $set_pb (param $idx i32) (param $val i32)
    (i32.store8 (i32.add (i32.const 185) (local.get $idx)) (local.get $val)))

  (func $init_pb (export "init_pb")
    (local $i i32)
    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_u (local.get $i) (i32.const 26)))
        (call $set_pb (local.get $i) (local.get $i))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop))))

  (func $set_pb_pair (export "set_pb_pair") (param $a i32) (param $b i32)
    (call $set_pb (local.get $a) (local.get $b))
    (call $set_pb (local.get $b) (local.get $a)))

  (func $pass_fwd (param $rotor i32) (param $offset i32) (param $ch i32) (result i32)
    (local $inp i32)
    (local $out i32)
    (local.set $inp (call $mod26 (i32.add (local.get $ch) (local.get $offset))))
    (local.set $out (call $get_fwd (local.get $rotor) (local.get $inp)))
    (call $mod26 (i32.add (i32.sub (local.get $out) (local.get $offset)) (i32.const 26))))

  (func $pass_bwd (param $rotor i32) (param $offset i32) (param $ch i32) (result i32)
    (local $inp i32)
    (local $out i32)
    (local.set $inp (call $mod26 (i32.add (local.get $ch) (local.get $offset))))
    (local.set $out (call $get_bwd (local.get $rotor) (local.get $inp)))
    (call $mod26 (i32.add (i32.sub (local.get $out) (local.get $offset)) (i32.const 26))))

  (func $encrypt_char (export "encrypt_char")
    (param $r0 i32) (param $r1 i32) (param $r2 i32)
    (param $o0_ptr i32) (param $o1_ptr i32) (param $o2_ptr i32)
    (param $ch i32)
    (result i32)
    (local $o0 i32) (local $o1 i32) (local $o2 i32)
    (local $mid i32) (local $atn i32) (local $c i32)
    ;; Load offsets
    (local.set $o0 (i32.load8_u (local.get $o0_ptr)))
    (local.set $o1 (i32.load8_u (local.get $o1_ptr)))
    (local.set $o2 (i32.load8_u (local.get $o2_ptr)))
    ;; Step
    (local.set $mid (i32.eq (local.get $o1) (call $get_notch (local.get $r1))))
    (local.set $atn (i32.eq (local.get $o2) (call $get_notch (local.get $r2))))
    (local.set $o2 (call $mod26 (i32.add (local.get $o2) (i32.const 1))))
    (if (i32.or (local.get $atn) (local.get $mid))
      (then (local.set $o1 (call $mod26 (i32.add (local.get $o1) (i32.const 1))))))
    (if (local.get $mid)
      (then (local.set $o0 (call $mod26 (i32.add (local.get $o0) (i32.const 1))))))
    ;; Store offsets
    (i32.store8 (local.get $o0_ptr) (local.get $o0))
    (i32.store8 (local.get $o1_ptr) (local.get $o1))
    (i32.store8 (local.get $o2_ptr) (local.get $o2))
    ;; Encrypt
    (local.set $c (call $get_pb (local.get $ch)))
    (local.set $c (call $pass_fwd (local.get $r2) (local.get $o2) (local.get $c)))
    (local.set $c (call $pass_fwd (local.get $r1) (local.get $o1) (local.get $c)))
    (local.set $c (call $pass_fwd (local.get $r0) (local.get $o0) (local.get $c)))
    (local.set $c (call $get_reflector (local.get $c)))
    (local.set $c (call $pass_bwd (local.get $r0) (local.get $o0) (local.get $c)))
    (local.set $c (call $pass_bwd (local.get $r1) (local.get $o1) (local.get $c)))
    (local.set $c (call $pass_bwd (local.get $r2) (local.get $o2) (local.get $c)))
    (local.set $c (call $get_pb (local.get $c)))
    (local.get $c))
)
