module investigate
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, investigate!"
  end subroutine say_hello
end module investigate
