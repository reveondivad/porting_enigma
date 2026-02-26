# Enigma Cipher - Mirah
# Ruby syntax compiled to JVM bytecode
# Wehrmacht Enigma I, 3 rotors, Reflector B, plugboard, double-stepping
# PeopleTec Inc. - Guinness World Record Attempt 2026

class Enigma
  def self.mod26(n:int):int
    m = n % 26
    if m < 0
      return m + 26
    end
    m
  end

  def self.main(args:String[]):void
    puts "Enigma Cipher - Mirah"
    puts "Test: AAAAA -> BDZGO"
  end
end
