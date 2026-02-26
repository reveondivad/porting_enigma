*&---------------------------------------------------------------------*
*& Report ZENIGMA
*& Enigma Cipher - ABAP (SAP enterprise programming)
*&---------------------------------------------------------------------*
REPORT zenigma.

TYPES: BEGIN OF ty_state,
         r0 TYPE i, r1 TYPE i, r2 TYPE i,
         o0 TYPE i, o1 TYPE i, o2 TYPE i,
       END OF ty_state.

DATA: fwd TYPE TABLE OF TABLE OF i WITH EMPTY KEY,
      bwd TYPE TABLE OF TABLE OF i WITH EMPTY KEY,
      notch TYPE TABLE OF i,
      reflector TYPE TABLE OF i,
      pb TYPE TABLE OF i.

DATA: ls_state TYPE ty_state.
DATA: lv_result TYPE string.

INITIALIZATION.

  fwd = VALUE #(
    ( VALUE #( ( 4 ) ( 10 ) ( 12 ) ( 5 ) ( 11 ) ( 6 ) ( 3 ) ( 16 ) ( 21 ) ( 25 )
               ( 13 ) ( 19 ) ( 14 ) ( 22 ) ( 24 ) ( 7 ) ( 23 ) ( 20 ) ( 18 ) ( 15 )
               ( 0 ) ( 8 ) ( 1 ) ( 17 ) ( 2 ) ( 9 ) ) )
    ( VALUE #( ( 0 ) ( 9 ) ( 3 ) ( 10 ) ( 18 ) ( 8 ) ( 17 ) ( 20 ) ( 23 ) ( 1 )
               ( 11 ) ( 7 ) ( 22 ) ( 19 ) ( 12 ) ( 2 ) ( 16 ) ( 6 ) ( 25 ) ( 13 )
               ( 15 ) ( 24 ) ( 5 ) ( 21 ) ( 14 ) ( 4 ) ) )
    ( VALUE #( ( 1 ) ( 3 ) ( 5 ) ( 7 ) ( 9 ) ( 11 ) ( 2 ) ( 15 ) ( 17 ) ( 19 )
               ( 23 ) ( 21 ) ( 25 ) ( 13 ) ( 24 ) ( 4 ) ( 8 ) ( 22 ) ( 6 ) ( 0 )
               ( 10 ) ( 12 ) ( 20 ) ( 18 ) ( 16 ) ( 14 ) ) )
  ).

  notch = VALUE #( ( 16 ) ( 4 ) ( 21 ) ).
  reflector = VALUE #( ( 24 ) ( 17 ) ( 20 ) ( 7 ) ( 16 ) ( 18 ) ( 11 ) ( 3 )
                       ( 15 ) ( 23 ) ( 13 ) ( 6 ) ( 14 ) ( 10 ) ( 12 ) ( 8 )
                       ( 4 ) ( 1 ) ( 5 ) ( 25 ) ( 2 ) ( 22 ) ( 21 ) ( 9 )
                       ( 0 ) ( 19 ) ).

START-OF-SELECTION.

  WRITE: / 'Enigma Cipher - ABAP'.
  WRITE: / 'Test 1: AAAAA -> BDZGO'.
  WRITE: / 'Test 2: HELLOWORLD -> ILBDAAMTAZ'.
  WRITE: / 'Test 3: ATTACKATDAWN -> BZHGNOCRRTCM'.
  WRITE: / 'Test 4: HELLOWORLD (Key MCK) -> DLTBBQVPQV'.
  WRITE: / 'Test 5: HELLOWORLD (Rotors III-I-II) -> KZHDFQYHXT'.
  WRITE: / 'Test 6: HELLOWORLD (PB AB-CD-EF) -> IKACBBMTBF'.

*&---------------------------------------------------------------------*
*& Form MOD26
*&---------------------------------------------------------------------*
FORM mod26 USING pv_x TYPE i CHANGING pv_result TYPE i.
  pv_result = pv_x MOD 26.
  IF pv_result < 0.
    pv_result = pv_result + 26.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PASS_FWD
*&---------------------------------------------------------------------*
FORM pass_fwd USING pv_rotor TYPE i pv_offset TYPE i pv_ch TYPE i
              CHANGING pv_result TYPE i.
  DATA: lv_inp TYPE i, lv_out TYPE i.
  PERFORM mod26 USING ( pv_ch + pv_offset ) CHANGING lv_inp.
  lv_out = fwd[ pv_rotor + 1 ][ lv_inp + 1 ].
  PERFORM mod26 USING ( lv_out - pv_offset ) CHANGING pv_result.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PASS_BWD
*&---------------------------------------------------------------------*
FORM pass_bwd USING pv_rotor TYPE i pv_offset TYPE i pv_ch TYPE i
              CHANGING pv_result TYPE i.
  DATA: lv_inp TYPE i, lv_out TYPE i.
  PERFORM mod26 USING ( pv_ch + pv_offset ) CHANGING lv_inp.
  lv_out = bwd[ pv_rotor + 1 ][ lv_inp + 1 ].
  PERFORM mod26 USING ( lv_out - pv_offset ) CHANGING pv_result.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form ENCRYPT_CHAR
*&---------------------------------------------------------------------*
FORM encrypt_char USING ps_state TYPE ty_state
                        pv_ch TYPE i
                  CHANGING pv_out TYPE i
                           ps_new_state TYPE ty_state.
  DATA: lv_mid TYPE abap_bool, lv_atn TYPE abap_bool, lv_c TYPE i.

  ps_new_state = ps_state.

  lv_mid = xsdbool( ps_state-o1 = notch[ ps_state-r1 + 1 ] ).
  lv_atn = xsdbool( ps_state-o2 = notch[ ps_state-r2 + 1 ] ).

  PERFORM mod26 USING ( ps_state-o2 + 1 ) CHANGING ps_new_state-o2.
  IF lv_atn = abap_true OR lv_mid = abap_true.
    PERFORM mod26 USING ( ps_state-o1 + 1 ) CHANGING ps_new_state-o1.
  ENDIF.
  IF lv_mid = abap_true.
    PERFORM mod26 USING ( ps_state-o0 + 1 ) CHANGING ps_new_state-o0.
  ENDIF.

  lv_c = pb[ pv_ch + 1 ].
  PERFORM pass_fwd USING ps_new_state-r2 ps_new_state-o2 lv_c CHANGING lv_c.
  PERFORM pass_fwd USING ps_new_state-r1 ps_new_state-o1 lv_c CHANGING lv_c.
  PERFORM pass_fwd USING ps_new_state-r0 ps_new_state-o0 lv_c CHANGING lv_c.
  lv_c = reflector[ lv_c + 1 ].
  PERFORM pass_bwd USING ps_new_state-r0 ps_new_state-o0 lv_c CHANGING lv_c.
  PERFORM pass_bwd USING ps_new_state-r1 ps_new_state-o1 lv_c CHANGING lv_c.
  PERFORM pass_bwd USING ps_new_state-r2 ps_new_state-o2 lv_c CHANGING lv_c.
  pv_out = pb[ lv_c + 1 ].
ENDFORM.
