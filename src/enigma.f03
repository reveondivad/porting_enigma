! Enigma cipher in Fortran 2003 (OOP features)
module enigma_mod
  implicit none
  private
  public :: EnigmaMachine

  type :: EnigmaMachine
    integer :: rotor_fwd(26,3)
    integer :: rotor_bwd(26,3)
    integer :: reflector(26)
    integer :: notches(3)
    integer :: pos(3)
  contains
    procedure :: init => enigma_init
    procedure :: mod26 => enigma_mod26
    procedure :: rotor_pass => enigma_rotor_pass
    procedure :: encrypt => enigma_encrypt
  end type

contains
  subroutine enigma_init(self)
    class(EnigmaMachine), intent(inout) :: self
    self%rotor_fwd(:,1) = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]
    self%rotor_fwd(:,2) = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]
    self%rotor_fwd(:,3) = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]
    self%rotor_bwd(:,1) = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]
    self%rotor_bwd(:,2) = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]
    self%rotor_bwd(:,3) = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]
    self%reflector = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]
    self%notches = [16, 4, 21]
    self%pos = [0, 0, 0]
  end subroutine

  pure integer function enigma_mod26(self, n)
    class(EnigmaMachine), intent(in) :: self
    integer, intent(in) :: n
    enigma_mod26 = modulo(n, 26)
  end function

  integer function enigma_rotor_pass(self, wiring, c, p)
    class(EnigmaMachine), intent(in) :: self
    integer, intent(in) :: wiring(26), c, p
    enigma_rotor_pass = self%mod26(wiring(self%mod26(c + p) + 1) - p)
  end function

  function enigma_encrypt(self, text) result(res)
    class(EnigmaMachine), intent(inout) :: self
    character(len=*), intent(in) :: text
    character(len=len(text)) :: res
    integer :: i, c, j
    logical :: mid
    self%pos = [0, 0, 0]
    res = ''
    do i = 1, len(text)
      c = iachar(text(i:i)) - 65
      if (c < 0 .or. c > 25) cycle
      mid = self%pos(2) == self%notches(2)
      if (self%pos(3) == self%notches(3)) self%pos(3) = self%mod26(self%pos(3) + 1)
      if (mid .or. self%pos(3) == self%notches(3)) self%pos(2) = self%mod26(self%pos(2) + 1)
      self%pos(3) = self%mod26(self%pos(3) + 1)
      do j = 3, 1, -1
        c = self%rotor_pass(self%rotor_fwd(:,j), c, self%pos(j))
      end do
      c = self%reflector(c + 1)
      do j = 1, 3
        c = self%rotor_pass(self%rotor_bwd(:,j), c, self%pos(j))
      end do
      res(i:i) = achar(c + 65)
    end do
  end function
end module

program main
  use enigma_mod
  type(EnigmaMachine) :: e
  call e%init()
  print *, e%encrypt("HELLOWORLD")
end program
