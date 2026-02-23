! Enigma Machine — Rosetta Code Reference (Fortran)
! Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
! PeopleTec Inc. — Guinness World Record Attempt 2026
program enigma_test
  implicit none
  character(26), parameter :: FWD(3) = (/ &
    'EKMFLGDQVZNTOWYHXUSPAIBRCJ', &
    'AJDKSIRUXBLHWTMCQGZNPYFVOE', &
    'BDFHJLCPRTXVZNYEIWGAKMUSQO' /)
  character(26), parameter :: BWD(3) = (/ &
    'UWYGADFPVZBECKMTHXSLRINQOJ', &
    'AJPCZWRLFBDKOTYUQGENHXMIVS', &
    'TAGBPCSDQEUFVNZHYIXJWLRKOM' /)
  integer, parameter :: NOTCH(3) = (/ ichar('Q'), ichar('E'), ichar('V') /)
  character(26), parameter :: REFL = 'YRUHQSLDPXNGOKMIEBFZCWVJAT'

  integer :: id(3), off(3), plug(0:25)
  logical :: all_ok

  all_ok = .true.
  call run_test(1, (/1,2,3/), 'AAA', 0, '', 'AAAAA', 'BDZGO')
  call run_test(2, (/1,2,3/), 'AAA', 0, '', 'HELLOWORLD', 'ILBDAAMTAZ')
  call run_test(3, (/1,2,3/), 'AAA', 0, '', 'ATTACKATDAWN', 'BZHGNOCRRTCM')
  call run_test(4, (/1,2,3/), 'MCK', 0, '', 'HELLOWORLD', 'DLTBBQVPQV')
  call run_test(5, (/3,1,2/), 'AAA', 0, '', 'HELLOWORLD', 'KZHDFQYHXT')
  call run_test(6, (/1,2,3/), 'AAA', 3, 'ABCDEF', 'HELLOWORLD', 'IKACBBMTBF')

  if (all_ok) then
    print *, 'ALL 6 TESTS PASSED'
  else
    print *, 'SOME TESTS FAILED'
  end if

contains

  function ci(c) result(r)
    character, intent(in) :: c
    integer :: r
    r = ichar(c) - 65
  end function

  subroutine init_enigma(rotors, key, np, plugstr)
    integer, intent(in) :: rotors(3), np
    character(3), intent(in) :: key
    character(*), intent(in) :: plugstr
    integer :: i, a, b
    id = rotors
    do i = 1, 3
      off(i) = ci(key(i:i))
    end do
    do i = 0, 25
      plug(i) = i
    end do
    do i = 1, np
      a = ci(plugstr(2*i-1:2*i-1))
      b = ci(plugstr(2*i:2*i))
      plug(a) = b; plug(b) = a
    end do
  end subroutine

  function fwd_pass(r, i) result(res)
    integer, intent(in) :: r, i
    integer :: res, c
    c = mod(i + off(r), 26)
    res = mod(ci(FWD(id(r))(c+1:c+1)) - off(r) + 26, 26)
  end function

  function bwd_pass(r, i) result(res)
    integer, intent(in) :: r, i
    integer :: res, c
    c = mod(i + off(r), 26)
    res = mod(ci(BWD(id(r))(c+1:c+1)) - off(r) + 26, 26)
  end function

  subroutine step_rotors()
    if (off(2) + 65 == NOTCH(id(2))) then
      off(2) = mod(off(2)+1, 26); off(1) = mod(off(1)+1, 26)
    else if (off(3) + 65 == NOTCH(id(3))) then
      off(2) = mod(off(2)+1, 26)
    end if
    off(3) = mod(off(3)+1, 26)
  end subroutine

  function press_key(ch) result(res)
    character, intent(in) :: ch
    character :: res
    integer :: i
    call step_rotors()
    i = ci(ch)
    i = plug(i)
    i = fwd_pass(3,i); i = fwd_pass(2,i); i = fwd_pass(1,i)
    i = ci(REFL(i+1:i+1))
    i = bwd_pass(1,i); i = bwd_pass(2,i); i = bwd_pass(3,i)
    i = plug(i)
    res = char(i + 65)
  end function

  function encrypt(text) result(out)
    character(*), intent(in) :: text
    character(len(text)) :: out
    integer :: j, k
    character :: c
    k = 0; out = ''
    do j = 1, len_trim(text)
      c = text(j:j)
      if (c >= 'A' .and. c <= 'Z') then
        k = k + 1; out(k:k) = press_key(c)
      end if
    end do
  end function

  subroutine run_test(num, rotors, key, np, plugstr, plain, expected)
    integer, intent(in) :: num, rotors(3), np
    character(*), intent(in) :: key, plugstr, plain, expected
    character(50) :: ct
    logical :: ok
    call init_enigma(rotors, key, np, plugstr)
    ct = encrypt(plain)
    ok = trim(ct) == trim(expected)
    write(*,'(A,I1,A,A20,A,A15,A,A,A)') 'Test ', num, ': ', plain, ' -> ', trim(ct), &
      ' [', merge('PASS','FAIL',ok), ']'
    if (.not. ok) all_ok = .false.
  end subroutine

end program
