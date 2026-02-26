# Enigma cipher in Elixir (proper .ex extension)
defmodule Enigma do
  @rotor_fwd [
    [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
    [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
    [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
  ]
  @rotor_bwd [
    [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9],
    [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18],
    [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
  ]
  @reflector [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
  @notches [16, 4, 21]

  def mod26(n), do: rem(rem(n, 26) + 26, 26)

  def rotor_pass(wiring, c, pos) do
    mod26(Enum.at(wiring, mod26(c + pos)) - pos)
  end

  def step_rotors([p0, p1, p2]) do
    mid = p1 == Enum.at(@notches, 1)
    p2 = if p2 == Enum.at(@notches, 2), do: mod26(p2 + 1), else: p2
    p1 = if mid or p2 == Enum.at(@notches, 2), do: mod26(p1 + 1), else: p1
    p2 = mod26(p2 + 1)
    [p0, p1, p2]
  end

  def encrypt_char(c, [p0, p1, p2]) do
    c = Enum.reduce(2..0//-1, c, fn i, acc ->
      rotor_pass(Enum.at(@rotor_fwd, i), acc, Enum.at([p0,p1,p2], i))
    end)
    c = Enum.at(@reflector, c)
    Enum.reduce(0..2, c, fn i, acc ->
      rotor_pass(Enum.at(@rotor_bwd, i), acc, Enum.at([p0,p1,p2], i))
    end)
  end

  def encrypt(text) do
    text
    |> String.upcase()
    |> String.to_charlist()
    |> Enum.filter(&(&1 >= ?A and &1 <= ?Z))
    |> Enum.map_reduce([0,0,0], fn ch, pos ->
      pos = step_rotors(pos)
      c = encrypt_char(ch - ?A, pos)
      {c + ?A, pos}
    end)
    |> elem(0)
    |> List.to_string()
  end
end

IO.puts(Enigma.encrypt("HELLOWORLD"))
