# Enigma Machine - Crystal Implementation
# Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
# PeopleTec Inc. - Guinness World Record Attempt 2026

FWD = ["EKMFLGDQVZNTOWYHXUSPAIBRCJ",
       "AJDKSIRUXBLHWTMCQGZNPYFVOE",
       "BDFHJLCPRTXVZNYEIWGAKMUSQO"]
BWD = ["UWYGADFPVZBECKMTHXSLRINQOJ",
       "AJPCZWRLFBDKOTYUQGENHXMIVS",
       "TAGBPCSDQEUFVNZHYIXJWLRKOM"]
NOTCH = [16, 4, 21]
REFL  = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

def mod26(a : Int32) : Int32
  ((a % 26) + 26) % 26
end

class Rotor
  property offset : Int32
  getter fwd : String, bwd : String, notch : Int32

  def initialize(num : Int32, win : Char)
    @fwd = FWD[num]; @bwd = BWD[num]; @notch = NOTCH[num]
    @offset = (win.ord - 'A'.ord).to_i32
  end

  def at_notch? : Bool
    @offset == @notch
  end

  def step
    @offset = (@offset + 1) % 26
  end

  def forward_pass(idx : Int32) : Int32
    contact = mod26(idx + @offset)
    mod26((@fwd[contact].ord - 'A'.ord).to_i32 - @offset)
  end

  def backward_pass(idx : Int32) : Int32
    contact = mod26(idx + @offset)
    mod26((@bwd[contact].ord - 'A'.ord).to_i32 - @offset)
  end
end

class Enigma
  def initialize(rotors : Array(Int32), key : String, plugboard : Array(String) = [] of String)
    @left = Rotor.new(rotors[0], key[0])
    @middle = Rotor.new(rotors[1], key[1])
    @right = Rotor.new(rotors[2], key[2])
    @plug = Array(Int32).new(26) { |i| i.to_i32 }
    plugboard.each do |pair|
      a = (pair[0].ord - 'A'.ord).to_i32
      b = (pair[1].ord - 'A'.ord).to_i32
      @plug[a] = b; @plug[b] = a
    end
  end

  def step_rotors
    if @middle.at_notch?
      @middle.step; @left.step
    elsif @right.at_notch?
      @middle.step
    end
    @right.step
  end

  def press_key(c : Char) : Char
    step_rotors
    idx = (c.ord - 'A'.ord).to_i32
    idx = @plug[idx]
    idx = @right.forward_pass(idx)
    idx = @middle.forward_pass(idx)
    idx = @left.forward_pass(idx)
    idx = (REFL[idx].ord - 'A'.ord).to_i32
    idx = @left.backward_pass(idx)
    idx = @middle.backward_pass(idx)
    idx = @right.backward_pass(idx)
    idx = @plug[idx]
    ('A'.ord + idx).chr
  end

  def encrypt(text : String) : String
    text.upcase.chars.select(&.ascii_letter?).map { |c| press_key(c) }.join
  end
end

puts "Enigma Machine - Crystal Implementation"
puts "========================================"

tests = [
  {[0, 1, 2].map(&.to_i32), "AAA", [] of String,         "AAAAA",        "BDZGO"},
  {[0, 1, 2].map(&.to_i32), "AAA", [] of String,         "HELLOWORLD",   "ILBDAAMTAZ"},
  {[0, 1, 2].map(&.to_i32), "AAA", [] of String,         "ATTACKATDAWN", "BZHGNOCRRTCM"},
  {[0, 1, 2].map(&.to_i32), "MCK", [] of String,         "HELLOWORLD",   "DLTBBQVPQV"},
  {[2, 0, 1].map(&.to_i32), "AAA", [] of String,         "HELLOWORLD",   "KZHDFQYHXT"},
  {[0, 1, 2].map(&.to_i32), "AAA", ["AB", "CD", "EF"],   "HELLOWORLD",   "IKACBBMTBF"},
]

all_pass = true
tests.each_with_index do |t, i|
  rotors, key, plugs, plain, expected = t
  e = Enigma.new(rotors, key, plugs)
  cipher = e.encrypt(plain)
  ok = cipher == expected
  status = ok ? "PASS" : "FAIL"
  printf "  Test %d: %-20s -> %-15s [%s]\n", i + 1, plain, cipher, status
  unless ok
    printf "          Expected %s, got %s\n", expected, cipher
    all_pass = false
  end
end
puts all_pass ? "\n  ALL 6 TESTS PASSED" : "\n  SOME TESTS FAILED"
