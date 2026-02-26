/* Enigma cipher in REXX/CMD (OS/2 REXX variant) */
rf1 = "4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9"
rf2 = "0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4"
rf3 = "1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14"
rb1 = "20 22 24 6 0 3 5 15 21 25 1 4 2 10 12 19 7 23 18 11 17 8 13 16 14 9"
rb2 = "0 9 15 2 25 22 17 11 5 1 3 10 14 19 24 20 16 6 4 13 7 23 12 8 21 18"
rb3 = "19 0 6 1 15 2 18 3 16 4 20 5 21 13 25 7 24 8 23 9 22 11 17 10 14 12"
ref = "24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19"
notches = "16 4 21"

p0 = 0; p1 = 0; p2 = 0
text = "HELLOWORLD"
result = ""

do i = 1 to length(text)
  c = c2d(substr(text, i, 1)) - 65
  if c < 0 | c > 25 then iterate
  mid = (p1 = word(notches, 2))
  if p2 = word(notches, 3) then p2 = mod26(p2 + 1)
  if mid | p2 = word(notches, 3) then p1 = mod26(p1 + 1)
  p2 = mod26(p2 + 1)
  c = mod26(word(rf3, mod26(c + p2) + 1) - p2)
  c = mod26(word(rf2, mod26(c + p1) + 1) - p1)
  c = mod26(word(rf1, mod26(c + p0) + 1) - p0)
  c = word(ref, c + 1)
  c = mod26(word(rb1, mod26(c + p0) + 1) - p0)
  c = mod26(word(rb2, mod26(c + p1) + 1) - p1)
  c = mod26(word(rb3, mod26(c + p2) + 1) - p2)
  result = result || d2c(c + 65)
end
say result
exit

mod26: procedure
  parse arg n
  m = n // 26
  if m < 0 then m = m + 26
  return m
