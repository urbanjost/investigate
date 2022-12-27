program main
  use M_OS, only: get_os_type, separator, which
  use M_OS, only: read_cmd, string_t
  use M_OS, only: debug_m_os
  implicit none
  character(len=*),parameter :: gen='(*(g0,1x))'
  integer :: os
  type(string_t), allocatable :: output(:)
  integer :: i

  debug_m_os=.true.

  os=get_os_type()
  write(*,gen)'get_os_type ==>',os

  write(*,gen)'separator ==>',separator()

  write(*,gen)'which uname ==>',which('uname')
  write(*,gen)'which systeminfo ==>',which('systeminfo')

  if(which('uname').ne.'')then
     call read_cmd('uname -a',output)
     do i=1,size(output)
        write(*,gen)output(i)%s
     enddo
  endif
  if(which('systeminfo').ne.'')then
     call read_cmd('systeminfo',output)
     do i=1,size(output)
        write(*,gen)output(i)%s
     enddo
  endif
end program main
