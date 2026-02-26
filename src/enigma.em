-- Enigma cipher in Emerald (distributed object language)
const EnigmaMachine <- object EnigmaMachine
  const rotorFwd1 <- Array.of[Integer].literal[4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
  const rotorFwd2 <- Array.of[Integer].literal[0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
  const rotorFwd3 <- Array.of[Integer].literal[1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
  const rotorBwd1 <- Array.of[Integer].literal[20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
  const rotorBwd2 <- Array.of[Integer].literal[0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
  const rotorBwd3 <- Array.of[Integer].literal[19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
  const reflector <- Array.of[Integer].literal[24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
  const notches <- Array.of[Integer].literal[16, 4, 21]

  operation mod26[n : Integer] -> [r : Integer]
    r <- ((n # 26) + 26) # 26
  end mod26

  operation rotorPass[wiring : Array.of[Integer], c : Integer, pos : Integer] -> [r : Integer]
    r <- self.mod26[wiring.getElement[self.mod26[c + pos]] - pos]
  end rotorPass

  operation encrypt[text : String] -> [result : String]
    const pos <- Array.of[Integer].literal[0, 0, 0]
    result <- ""
    for i : Integer <- 0 while i < text.length by i <- i + 1
      const ch <- text.getElement[i].ord - 65
      if ch >= 0 and ch < 26 then
        var c : Integer <- ch
        for j : Integer <- 2 while j >= 0 by j <- j - 1
          c <- self.rotorPass[{rotorFwd1, rotorFwd2, rotorFwd3}[j], c, pos.getElement[j]]
        end for
        c <- reflector.getElement[c]
        for j : Integer <- 0 while j <= 2 by j <- j + 1
          c <- self.rotorPass[{rotorBwd1, rotorBwd2, rotorBwd3}[j], c, pos.getElement[j]]
        end for
        result <- result || (c + 65).asCharacter.asString
      end if
    end for
  end encrypt

  initially
    stdout.putString[self.encrypt["HELLOWORLD"]]
    stdout.putChar['\n']
  end initially
end EnigmaMachine
