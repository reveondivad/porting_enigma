# Enigma Machine - Elixir Implementation
# Wehrmacht Enigma I (3-rotor, Reflector B, plugboard, double-stepping)
# PeopleTec Inc. - Guinness World Record Attempt 2026

defmodule Enigma do
  @fwd %{
    1 => 'EKMFLGDQVZNTOWYHXUSPAIBRCJ',
    2 => 'AJDKSIRUXBLHWTMCQGZNPYFVOE',
    3 => 'BDFHJLCPRTXVZNYEIWGAKMUSQO'
  }
  @bwd %{
    1 => 'UWYGADFPVZBECKMTHXSLRINQOJ',
    2 => 'AJPCZWRLFBDKOTYUQGENHXMIVS',
    3 => 'TAGBPCSDQEUFVNZHYIXJWLRKOM'
  }
  @notch %{1 => 16, 2 => 4, 3 => 21}
  @refl 'YRUHQSLDPXNGOKMIEBFZCWVJAT'

  defp mod26(a), do: rem(rem(a, 26) + 26, 26)

  defp fwd_pass(wiring, offset, idx) do
    contact = mod26(idx + offset)
    out = Enum.at(wiring, contact) - ?A
    mod26(out - offset)
  end

  defp bwd_pass(wiring, offset, idx) do
    contact = mod26(idx + offset)
    out = Enum.at(wiring, contact) - ?A
    mod26(out - offset)
  end

  def new_machine(rotors, key, plugboard \\ []) do
    [r1, r2, r3] = rotors
    [k1, k2, k3] = String.to_charlist(key)

    plug = Enum.into(0..25, %{}, fn i -> {i, i} end)
    plug = Enum.reduce(plugboard, plug, fn pair, acc ->
      [a, b] = String.to_charlist(pair)
      a = a - ?A; b = b - ?A
      acc |> Map.put(a, b) |> Map.put(b, a)
    end)

    %{
      left: %{fwd: @fwd[r1], bwd: @bwd[r1], notch: @notch[r1], offset: k1 - ?A},
      middle: %{fwd: @fwd[r2], bwd: @bwd[r2], notch: @notch[r2], offset: k2 - ?A},
      right: %{fwd: @fwd[r3], bwd: @bwd[r3], notch: @notch[r3], offset: k3 - ?A},
      plug: plug
    }
  end

  defp step_rotors(m) do
    cond do
      m.middle.offset == m.middle.notch ->
        %{m |
          middle: %{m.middle | offset: rem(m.middle.offset + 1, 26)},
          left: %{m.left | offset: rem(m.left.offset + 1, 26)}}
      m.right.offset == m.right.notch ->
        %{m | middle: %{m.middle | offset: rem(m.middle.offset + 1, 26)}}
      true -> m
    end
    |> then(fn m -> %{m | right: %{m.right | offset: rem(m.right.offset + 1, 26)}} end)
  end

  defp press_key(m, char) do
    m = step_rotors(m)
    idx = char - ?A
    idx = m.plug[idx]
    idx = fwd_pass(m.right.fwd, m.right.offset, idx)
    idx = fwd_pass(m.middle.fwd, m.middle.offset, idx)
    idx = fwd_pass(m.left.fwd, m.left.offset, idx)
    idx = Enum.at(@refl, idx) - ?A
    idx = bwd_pass(m.left.bwd, m.left.offset, idx)
    idx = bwd_pass(m.middle.bwd, m.middle.offset, idx)
    idx = bwd_pass(m.right.bwd, m.right.offset, idx)
    idx = m.plug[idx]
    {m, idx + ?A}
  end

  def encrypt(m, text) do
    text
    |> String.upcase()
    |> String.to_charlist()
    |> Enum.filter(&(&1 >= ?A and &1 <= ?Z))
    |> Enum.reduce({m, []}, fn char, {m, acc} ->
      {m, out} = press_key(m, char)
      {m, [out | acc]}
    end)
    |> then(fn {_, chars} -> chars |> Enum.reverse() |> List.to_string() end)
  end
end

# Test harness
IO.puts("Enigma Machine - Elixir Implementation")
IO.puts("=======================================")

tests = [
  {[1,2,3], "AAA", [],              "AAAAA",        "BDZGO"},
  {[1,2,3], "AAA", [],              "HELLOWORLD",   "ILBDAAMTAZ"},
  {[1,2,3], "AAA", [],              "ATTACKATDAWN", "BZHGNOCRRTCM"},
  {[1,2,3], "MCK", [],              "HELLOWORLD",   "DLTBBQVPQV"},
  {[3,1,2], "AAA", [],              "HELLOWORLD",   "KZHDFQYHXT"},
  {[1,2,3], "AAA", ["AB","CD","EF"],"HELLOWORLD",   "IKACBBMTBF"}
]

all_pass = tests
|> Enum.with_index(1)
|> Enum.reduce(true, fn {{rotors, key, plugs, plain, expected}, i}, acc ->
  m = Enigma.new_machine(rotors, key, plugs)
  cipher = Enigma.encrypt(m, plain)
  ok = cipher == expected
  status = if ok, do: "PASS", else: "FAIL"
  IO.puts("  Test #{i}: #{String.pad_trailing(plain, 20)} -> #{String.pad_trailing(cipher, 15)} [#{status}]")
  unless ok, do: IO.puts("          Expected #{expected}, got #{cipher}")
  acc and ok
end)

IO.puts(if all_pass, do: "\n  ALL 6 TESTS PASSED", else: "\n  SOME TESTS FAILED")
