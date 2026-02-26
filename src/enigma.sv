// Enigma Cipher - SystemVerilog Implementation
// Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
// PeopleTec Inc. - Guinness World Record Attempt 2026
// Simulation testbench using SystemVerilog constructs

module enigma_tb;

  // Rotor wirings as packed arrays
  typedef logic [4:0] alpha_t;
  typedef alpha_t wiring_t [0:25];

  wiring_t fwd_I = '{5'd4,5'd10,5'd12,5'd5,5'd11,5'd6,5'd3,5'd16,5'd21,5'd25,
                      5'd13,5'd19,5'd14,5'd22,5'd24,5'd7,5'd23,5'd20,5'd18,5'd15,
                      5'd0,5'd8,5'd1,5'd17,5'd2,5'd9};
  wiring_t fwd_II = '{5'd0,5'd9,5'd3,5'd10,5'd18,5'd8,5'd17,5'd20,5'd23,5'd1,
                       5'd11,5'd7,5'd22,5'd19,5'd12,5'd2,5'd16,5'd6,5'd25,5'd13,
                       5'd15,5'd24,5'd5,5'd21,5'd14,5'd4};
  wiring_t fwd_III = '{5'd1,5'd3,5'd5,5'd7,5'd9,5'd11,5'd2,5'd15,5'd17,5'd19,
                        5'd23,5'd21,5'd25,5'd13,5'd24,5'd4,5'd8,5'd22,5'd6,5'd0,
                        5'd10,5'd12,5'd20,5'd18,5'd16,5'd14};
  wiring_t bwd_I = '{5'd20,5'd22,5'd24,5'd6,5'd0,5'd3,5'd5,5'd15,5'd21,5'd25,
                      5'd1,5'd4,5'd2,5'd10,5'd12,5'd19,5'd7,5'd23,5'd18,5'd11,
                      5'd17,5'd8,5'd13,5'd16,5'd14,5'd9};
  wiring_t bwd_II = '{5'd0,5'd9,5'd15,5'd2,5'd25,5'd22,5'd17,5'd11,5'd5,5'd1,
                       5'd3,5'd10,5'd14,5'd19,5'd24,5'd20,5'd16,5'd6,5'd4,5'd13,
                       5'd7,5'd23,5'd12,5'd8,5'd21,5'd18};
  wiring_t bwd_III = '{5'd19,5'd0,5'd6,5'd1,5'd15,5'd2,5'd18,5'd3,5'd16,5'd4,
                        5'd20,5'd5,5'd21,5'd13,5'd25,5'd7,5'd24,5'd8,5'd23,5'd9,
                        5'd22,5'd11,5'd17,5'd10,5'd14,5'd12};
  wiring_t reflector = '{5'd24,5'd17,5'd20,5'd7,5'd16,5'd18,5'd11,5'd3,5'd15,5'd23,
                          5'd13,5'd6,5'd14,5'd10,5'd12,5'd8,5'd4,5'd1,5'd5,5'd25,
                          5'd2,5'd22,5'd21,5'd9,5'd0,5'd19};
  alpha_t notch [0:2] = '{5'd16, 5'd4, 5'd21};

  function automatic alpha_t mod26(int n);
    int m = n % 26;
    if (m < 0) m += 26;
    return alpha_t'(m);
  endfunction

  function automatic alpha_t fwd_pass(int rotor, alpha_t offset, alpha_t ch);
    alpha_t contact = mod26(int'(ch) + int'(offset));
    alpha_t out_val;
    case (rotor)
      0: out_val = fwd_I[contact];
      1: out_val = fwd_II[contact];
      2: out_val = fwd_III[contact];
      default: out_val = 0;
    endcase
    return mod26(int'(out_val) - int'(offset));
  endfunction

  function automatic alpha_t bwd_pass(int rotor, alpha_t offset, alpha_t ch);
    alpha_t contact = mod26(int'(ch) + int'(offset));
    alpha_t out_val;
    case (rotor)
      0: out_val = bwd_I[contact];
      1: out_val = bwd_II[contact];
      2: out_val = bwd_III[contact];
      default: out_val = 0;
    endcase
    return mod26(int'(out_val) - int'(offset));
  endfunction

  task automatic encrypt(
    input int r0, r1, r2, k0, k1, k2,
    input string msg,
    output string result
  );
    alpha_t o0, o1, o2, n1, n2;
    alpha_t pb [0:25];
    alpha_t c;
    bit mid, atn;

    o0 = alpha_t'(k0); o1 = alpha_t'(k1); o2 = alpha_t'(k2);
    n1 = notch[r1]; n2 = notch[r2];
    for (int i = 0; i < 26; i++) pb[i] = alpha_t'(i);
    result = "";

    for (int i = 0; i < msg.len(); i++) begin
      alpha_t ch = alpha_t'(msg[i] - "A");
      mid = (o1 == n1);
      atn = (o2 == n2);
      o2 = mod26(int'(o2) + 1);
      if (atn || mid) o1 = mod26(int'(o1) + 1);
      if (mid) o0 = mod26(int'(o0) + 1);
      c = pb[ch];
      c = fwd_pass(r2, o2, c);
      c = fwd_pass(r1, o1, c);
      c = fwd_pass(r0, o0, c);
      c = reflector[c];
      c = bwd_pass(r0, o0, c);
      c = bwd_pass(r1, o1, c);
      c = bwd_pass(r2, o2, c);
      c = pb[c];
      result = {result, string'(byte'(int'(c) + 65))};
    end
  endtask

  initial begin
    string res;
    $display("Enigma Cipher - SystemVerilog");
    encrypt(0,1,2, 0,0,0, "AAAAA", res);
    $display("Test 1: %s %s", res, res == "BDZGO" ? "[PASS]" : "[FAIL]");
    encrypt(0,1,2, 0,0,0, "HELLOWORLD", res);
    $display("Test 2: %s %s", res, res == "ILBDAAMTAZ" ? "[PASS]" : "[FAIL]");
    encrypt(0,1,2, 0,0,0, "ATTACKATDAWN", res);
    $display("Test 3: %s %s", res, res == "BZHGNOCRRTCM" ? "[PASS]" : "[FAIL]");
    encrypt(0,1,2, 12,2,10, "HELLOWORLD", res);
    $display("Test 4: %s %s", res, res == "DLTBBQVPQV" ? "[PASS]" : "[FAIL]");
    encrypt(2,0,1, 0,0,0, "HELLOWORLD", res);
    $display("Test 5: %s %s", res, res == "KZHDFQYHXT" ? "[PASS]" : "[FAIL]");
    $finish;
  end
endmodule
