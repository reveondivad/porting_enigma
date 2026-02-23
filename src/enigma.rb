# Enigma Machine — Rosetta Code Reference (Ruby)
# Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
# PeopleTec Inc. — Guinness World Record Attempt 2026

FWD = %w[EKMFLGDQVZNTOWYHXUSPAIBRCJ AJDKSIRUXBLHWTMCQGZNPYFVOE BDFHJLCPRTXVZNYEIWGAKMUSQO]
BWD = %w[UWYGADFPVZBECKMTHXSLRINQOJ AJPCZWRLFBDKOTYUQGENHXMIVS TAGBPCSDQEUFVNZHYIXJWLRKOM]
NOTCH = %w[Q E V]
REFLECTOR = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

class Enigma
  def initialize(rotors, key, plugboard = [])
    @id, @off = rotors, key.chars.map { |c| c.ord - 65 }
    @plug = (0..25).to_a
    plugboard.each { |p| a, b = p[0].ord-65, p[1].ord-65; @plug[a], @plug[b] = b, a }
  end

  def fwd(r, i) = ((FWD[@id[r]][(i+@off[r])%26].ord-65) - @off[r] + 26) % 26
  def bwd(r, i) = ((BWD[@id[r]][(i+@off[r])%26].ord-65) - @off[r] + 26) % 26

  def step
    if (@off[1]+65).chr == NOTCH[@id[1]]
      @off[1] = (@off[1]+1)%26; @off[0] = (@off[0]+1)%26
    elsif (@off[2]+65).chr == NOTCH[@id[2]]
      @off[1] = (@off[1]+1)%26
    end
    @off[2] = (@off[2]+1)%26
  end

  def press_key(ch)
    step; i = ch.ord - 65
    i = @plug[i]
    i = fwd(2,i); i = fwd(1,i); i = fwd(0,i)
    i = REFLECTOR[i].ord - 65
    i = bwd(0,i); i = bwd(1,i); i = bwd(2,i)
    i = @plug[i]
    (i+65).chr
  end

  def encrypt(text)
    text.upcase.gsub(/[^A-Z]/,'').chars.map { |c| press_key(c) }.join
  end
end

tests = [
  [[0,1,2],"AAA",[],"AAAAA","BDZGO"],
  [[0,1,2],"AAA",[],"HELLOWORLD","ILBDAAMTAZ"],
  [[0,1,2],"AAA",[],"ATTACKATDAWN","BZHGNOCRRTCM"],
  [[0,1,2],"MCK",[],"HELLOWORLD","DLTBBQVPQV"],
  [[2,0,1],"AAA",[],"HELLOWORLD","KZHDFQYHXT"],
  [[0,1,2],"AAA",%w[AB CD EF],"HELLOWORLD","IKACBBMTBF"],
]

all_ok = true
tests.each_with_index do |(r,k,p,pt,exp), t|
  ct = Enigma.new(r,k,p).encrypt(pt)
  ok = ct == exp
  printf "Test %d: %-20s -> %-15s [%s]\n", t+1, pt, ct, ok ? "PASS" : "FAIL"
  all_ok = false unless ok
end
puts all_ok ? "\nALL 6 TESTS PASSED" : "\nSOME TESTS FAILED"
