module M_OS
implicit none
private
public :: get_os_type
public :: separator
public :: which

public :: read_cmd
public :: string_t

logical,public,save :: debug_m_os=.false.

private :: substitute
private :: split
private :: lower
private :: get_env
private :: run
private :: exists
private :: number_of_rows
private :: read_lines
private :: joinpath

integer, parameter, public :: OS_UNKNOWN = 0
integer, parameter, public :: OS_LINUX   = 1
integer, parameter, public :: OS_MACOS   = 2
integer, parameter, public :: OS_WINDOWS = 3
integer, parameter, public :: OS_CYGWIN  = 4
integer, parameter, public :: OS_SOLARIS = 5
integer, parameter, public :: OS_FREEBSD = 6
! did not save properly with ifort(1) if defined in GET_OS_TYPE(3f)
integer,save,private       :: OS_CACHED=-1

type string_t
    character(len=:), allocatable :: s
end type

integer, parameter :: LINE_BUFFER_LEN = 1000

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!$BLOCK COMMENT -file lower.3m_strings.man
!>## NAME
!!  lower(3f) - [M_strings:CASE] changes a string to lowercase over
!!  specified range
!!  (LICENSE:PD)
!!
!!## SYNOPSIS
!!    elemental pure function lower(str,begin,end) result (string)
!!
!!    character(*), intent(in) :: str
!!    integer,optional         :: begin, end
!!    character(len(str))      :: string  ! output string
!!
!!## DESCRIPTION
!!  lower(string) returns a copy of the input string with all characters
!!  converted to miniscule over the specified range, assuming ASCII
!!  character sets are being used. If no range is specified the entire
!!  string is converted to miniscule.
!!
!!## OPTIONS
!!   str    string to convert to miniscule
!!   begin  optional starting position in "str" to begin converting to
!!          miniscule
!!   end    optional ending position in "str" to stop converting to
!!          miniscule
!!
!!## RESULTS
!!   lower  copy of the input string with all characters converted to
!!          miniscule over optionally specified range.
!!
!!## TRIVIA
!!  The terms "uppercase" and "lowercase" date back to the early days of
!!  the mechanical printing press. Individual metal alloy casts of each
!!  needed letter, or punctuation symbol, were meticulously added to a
!!  press block, by hand, before rolling out copies of a page. These
!!  metal casts were stored and organized in wooden cases. The more
!!  often needed miniscule letters were placed closer to hand, in the
!!  lower cases of the work bench. The less often needed, capitalized,
!!  majuscule letters, ended up in the harder to reach upper cases.
!!
!!## EXAMPLE
!!   Sample program:
!!
!!      program demo_lower
!!      use M_strings, only: lower
!!      implicit none
!!      character(len=:),allocatable  :: s
!!         s=' ABCDEFG abcdefg '
!!         write(*,*) 'mixed-case input string is ....',s
!!         write(*,*) 'lower-case output string is ...',lower(s)
!!      end program demo_lower
!!
!!   Expected output
!!
!!      mixed-case input string is .... ABCDEFG abcdefg
!!      lower-case output string is ... abcdefg abcdefg
!!
!!## AUTHOR
!!   John S. Urban
!!
!!## LICENSE
!!   Public Domain
!$BLOCK
!===================================================================================================================================
elemental pure function lower(str,begin,end) result (string)

!$@(#) M_strings::lower(3f): Changes a string to lowercase over specified range

character(*), intent(in)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(1,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                   ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))-diff)   ! change letter to miniscule
      case default
      end select
   enddo

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!$BLOCK COMMENT -file substitute.3m_strings.man
!>## NAME
!!   substitute(3f) - [M_strings:EDITING] subroutine globally substitutes
!!   one substring for another in string
!!   (LICENSE:PD)
!!
!!## SYNOPSIS
!!   subroutine substitute(targetline,old,new,ierr,start,end)
!!
!!    character(len=*)              :: targetline
!!    character(len=*),intent(in)   :: old
!!    character(len=*),intent(in)   :: new
!!    integer,intent(out),optional  :: ierr
!!    integer,intent(in),optional   :: start
!!    integer,intent(in),optional   :: end
!!
!!## DESCRIPTION
!!   Globally substitute one substring for another in string.
!!
!!## OPTIONS
!!    TARGETLINE  input line to be changed. Must be long enough to
!!                hold altered output.
!!    OLD         substring to find and replace
!!    NEW         replacement for OLD substring
!!    IERR        error code. If IER = -1 bad directive, >= 0 then
!!                count of changes made.
!!    START       sets the left margin to be scanned for OLD in
!!                TARGETLINE.
!!    END         sets the right margin to be scanned for OLD in
!!                TARGETLINE.
!!
!!## EXAMPLES
!!  Sample Program:
!!
!!   program demo_substitute
!!   use M_strings, only : substitute
!!   implicit none
!!   ! must be long enough to hold changed line
!!   character(len=80) :: targetline
!!
!!   targetline='this is the input string'
!!   write(*,*)'ORIGINAL    : '//trim(targetline)
!!
!!   ! changes the input to 'THis is THe input string'
!!   call substitute(targetline,'th','TH')
!!   write(*,*)'th => TH    : '//trim(targetline)
!!
!!   ! a null old substring means "at beginning of line"
!!   ! changes the input to 'BEFORE:this is the input string'
!!   call substitute(targetline,'','BEFORE:')
!!   write(*,*)'"" => BEFORE: '//trim(targetline)
!!
!!   ! a null new string deletes occurrences of the old substring
!!   ! changes the input to 'ths s the nput strng'
!!   call substitute(targetline,'i','')
!!   write(*,*)'i => ""     : '//trim(targetline)
!!
!!   end program demo_substitute
!!
!!  Expected output
!!
!!    ORIGINAL    : this is the input string
!!    th => TH    : THis is THe input string
!!    "" => BEFORE: BEFORE:THis is THe input string
!!    i => ""     : BEFORE:THs s THe nput strng
!!
!!## AUTHOR
!!   John S. Urban
!!
!!## LICENSE
!!   Public Domain
!$BLOCK
!===================================================================================================================================
subroutine substitute(targetline,old,new,ierr,start,end)

!$@(#) M_strings::substitute(3f): Globally substitute one substring for another in string

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*)               :: targetline         ! input line to be changed
character(len=*),intent(in)    :: old                ! old substring to replace
character(len=*),intent(in)    :: new                ! new substring
integer,intent(out),optional   :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional    :: start              ! start sets the left margin
integer,intent(in),optional    :: end                ! end sets the right margin
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(targetline)) :: dum1               ! scratch string buffers
integer                        :: ml, mr, ier1
integer                        :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: ichar
!-----------------------------------------------------------------------------------------------------------------------------------
   if (present(start)) then                            ! optional starting column
      ml=start
   else
      ml=1
   endif
   if (present(end)) then                              ! optional ending column
      mr=end
   else
      mr=len(targetline)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier1=0                                              ! initialize error flag/change count
   maxlengthout=len(targetline)                        ! max length of output string
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   dum1(:)=' '                                         ! initialize string to build output in
   id=mr-ml                                            ! check for window option !x! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id.le.0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(ichar.gt.maxlengthout)then
         write(*,'(a)')'*substitute* new line will be too long'
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new.gt.0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1                                           ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ier1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=il                                            ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(ichar-1+ladd.gt.maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichar:)=targetline(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(ichar-1+len_new.gt.maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new.ne.0)then
         dum1(ichar:)=new(:len_new)
         ichar=ichar+len_new
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ier1)
   case (:-1)
      write(*,'(a)')'*substitute* new line will be too long'
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(ichar+ladd.gt.maxlengthout)then
         write(*,'(a)')'*substitute* new line will be too long'
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic.lt.len(targetline))then
         dum1(ichar:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!$BLOCK COMMENT -file split.3m_strings.man
!>## NAME
!!   split(3f) - [M_strings:TOKENS] parse string into an array using
!!   specified delimiters
!!   (LICENSE:PD)
!!
!!## SYNOPSIS
!!   subroutine split(input_line,array,delimiters,order,nulls)
!!
!!    character(len=*),intent(in)              :: input_line
!!    character(len=:),allocatable,intent(out) :: array(:)
!!    character(len=*),optional,intent(in)     :: delimiters
!!    character(len=*),optional,intent(in)     :: order
!!    character(len=*),optional,intent(in)     :: nulls
!!
!!## DESCRIPTION
!!    SPLIT(3f) parses a string using specified delimiter characters and
!!    store tokens into an allocatable array
!!
!!## OPTIONS
!!   INPUT_LINE  Input string to tokenize
!!
!!   ARRAY       Output array of tokens
!!
!!   DELIMITERS  List of delimiter characters.
!!               The default delimiters are the "whitespace" characters
!!               (space, tab,new line, vertical tab, formfeed, carriage
!!               return, and null). You may specify an alternate set of
!!               delimiter characters.
!!
!!               Multi-character delimiters are not supported (Each
!!               character in the DELIMITERS list is considered to be
!!               a delimiter).
!!
!!               Quoting of delimiter characters is not supported.
!!
!!   NULLS=IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!               By default adjacent delimiters in the input string
!!               do not create an empty string in the output array. if
!!               NULLS='return' adjacent delimiters create an empty element
!!               in the output ARRAY. If NULLS='ignoreend' then only
!!               trailing delimiters at the right of the string are ignored.
!!
!!## EXAMPLES
!! Sample program:
!!
!!   program demo_split
!!   use M_strings, only: split
!!   implicit none
!!   integer :: i
!!   character(len=*),parameter     :: line=&
!!   '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!   character(len=:),allocatable :: array(:) ! output array of tokens
!!      write(*,*)'INPUT LINE:['//LINE//']'
!!      write(*,'(70("="))')
!!      write(*,*)'typical call:'
!!      CALL split(line,array)
!!      write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!      write(*,*)'SIZE:',SIZE(array)
!!      write(*,'(70("-"))')
!!      write(*,*)'custom list of delimiters (colon and vertical line):'
!!      CALL split(line,array,delimiters=':|',&
!!      & order='sequential',nulls='ignore')
!!      write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!      write(*,*)'SIZE:',SIZE(array)
!!      write(*,'(70("-"))')
!!      write(*,*) 'custom list of delimiters, count null fields:'
!!      CALL split(line,array,delimiters=':|',nulls='return')
!!      write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!      write(*,*)'SIZE:',SIZE(array)
!!      write(*,'(70("-"))')
!!      write(*,*)'INPUT LINE:['//LINE//']'
!!      write(*,*) 'default delimiters and reverse array order &
!!      &and return null fields:'
!!      CALL split(line,array,delimiters='',order='reverse',nulls='return')
!!      write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!      write(*,*)'SIZE:',SIZE(array)
!!   end program demo_split
!!
!!## AUTHOR
!!   John S. Urban
!!
!!## LICENSE
!!   Public Domain
!$BLOCK
!===================================================================================================================================
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

!$@(#) M_strings::split(3f): parse string on delimiter characters and store tokens into an allocatable array

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: icount                 ! number of tokens found
integer                       :: ilen                   ! length of input string with trailing spaces trimmed
integer                       :: ii
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)    !*! intel compiler says allocated already ?
   if(allocated(iterm))deallocate(iterm)      !*! intel compiler says allocated already ?
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.gt.0)then                                              ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol.gt.ilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                      ! allocate the array to return
!-----------------------------------------------------------------------------------------------------------------------------------
   ii=1                                                           ! first to last
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20).lt.ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+1
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+1
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!$FILTER COMMENT -file random_string.3m_random.man
!>## NAME
!!   random_string(3f) - [M_random] create random string composed of
!!                       provided characters of specified length
!!   (LICENSE:MIT)
!!
!!## SYNOPSIS
!!   function random_string(chars,length) result(out)
!!
!!    character(len=*),intent(in)     :: chars
!!    integer,intent(in)              :: length
!!    character(len=:),allocatable    :: out
!!
!!## DESCRIPTION
!!   Given a set of characters and a length, generate a random string of
!!   the specified length composed of the given set of characters.
!!
!!## OPTIONS
!!   chars   list of characters to generate random string with
!!   length  number of characters to place in output string
!!
!!## RESULT
!!   out     string of LENGTH characters randomly filled with characters
!!           from CHARS
!!
!!## EXAMPLE
!!   Sample program:
!!
!!    program demo_random_string
!!    use M_random, only : random_string, init_random_seed_by_dat
!!       character(len=64) :: hexstring
!!       ! use date and time to create a seed for calling random_seed(3f)
!!       call init_random_seed_by_dat()
!!       hexstring=random_string('0123456789abcdef',len(hexstring))
!!       ! write random hexadecimal value for use
!!       ! as something like an X11 authorization key
!!       write(*,'(a)')hexstring
!!    end program demo_random_string
!!
!!   Results
!!
!!    2363a3589736e23be0137ec7ebc9d74297a963f27958a176daea3dd850ed8487
!!
!!## AUTHOR
!!   John S. Urban
!!
!!## LICENSE
!!   MIT License
!$FILTER END
!===================================================================================================================================
function random_string(chars,length) result(out)

!$@(#) M_random::random_string(3f): create random string composed of provided characters of specified length

character(len=*),intent(in)     :: chars
integer,intent(in)              :: length
character(len=:),allocatable    :: out
   real                         :: x
   integer                      :: ilen   ! length of list of characters
   integer                      :: which
   integer                      :: i
   ilen=len(chars)
   out=''
   if(ilen.gt.0)then
      do i=1,length
         call random_number(x)
         which=nint(real(ilen-1)*x)+1
         out=out//chars(which:which)
      enddo
   endif
end function random_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function scratch() result(filename)
character(:), allocatable :: filename
integer                     :: ios
   filename=random_string('abcdefghijklmnopqrstuvwxyz_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',48)//'.scr'
end function scratch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function get_env(NAME,DEFAULT) result(VALUE)
implicit none
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
        ! get length required to hold value
        length=0
        if(NAME.ne.'')then
           call get_environment_variable(NAME, length=howbig,status=stat,trim_name=.true.)
           select case (stat)
           case (1)
               !*!print *, NAME, " is not defined in the environment. Strange..."
               VALUE=''
           case (2)
               !*!print *, "This processor doesn't support environment variables. Boooh!"
               VALUE=''
           case default
               ! make string to hold value of sufficient size
               allocate(character(len=max(howbig,1)) :: VALUE)
               ! get value
               call get_environment_variable(NAME,VALUE,status=stat,trim_name=.true.)
               if(stat.ne.0)VALUE=''
           end select
        else
           VALUE=''
        endif
        if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
end function get_env
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!> test if pathname already exists
logical function exists(filename) result(r)
    character(len=*), intent(in) :: filename
    inquire(file=filename, exist=r)
end function
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!> Determine number or rows in a file given a LUN
integer function number_of_rows(s) result(nrows)
integer,intent(in) ::s
integer            :: ios
character(len=100) :: r
    rewind(s)
    nrows = 0
    do
        read(s, '(A)', iostat=ios) r
        if (ios /= 0) exit
        nrows = nrows + 1
    enddo
    rewind(s)
end function number_of_rows
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!> read lines into an array of TYPE(STRING_T) variables
function read_lines(fh) result(lines)
integer, intent(in)         :: fh
type(string_t), allocatable :: lines(:)
integer                     :: i
character(LINE_BUFFER_LEN)  :: line_buffer

    allocate(lines(number_of_rows(fh)))
    do i = 1, size(lines)
        read(fh, '(A)') line_buffer
        lines(i)%s = trim(line_buffer)
    end do

end function read_lines
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!$BLOCK COMMENT -file joinpath.3m_io.man
!>## NAME
!!   joinpath(3f) - [M_io] join parts of a pathname together
!!   (LICENSE:PD)
!!
!!## SYNOPSIS
!!   function joinpath(a1,a2,a3,a4,a5,a6,a7,a8,a9)  result(path)
!!
!!    character(len=*), intent(in)           :: a1, a2
!!    character(len=*), intent(in), optional :: a3, a4, a5, a6, a7, a8, a9
!!    character(len=:), allocatable          :: path
!!## DESCRIPTION
!!## OPTIONS
!!    a1,a2  the first two pathname sections to join. Required
!!    a3-a9  additional optional sections to join
!!## RETURNS
!!    path   pathname sections joined together with trailing spaces removed from the ends
!!           of sections and a separator (as returned by separator(3f)) placed between
!!           them, and duplicate adjacent separators removed accept for one beginning the
!!           joined pathname.
!!## EXAMPLE
!!  Sample program
!!
!!     program demo_joinpath
!!     use M_io, only : joinpath
!!     implicit none
!!        write(*,*)joinpath('/share/user','/man/','man3','joinpath.3m_io'//'.gz')
!!     end program demo_joinpath
!!## AUTHOR
!!   John S. Urban
!!## LICENSE
!!   Public Domain
!$BLOCK
!===================================================================================================================================
!> Construct path by joining strings with os file separator
function joinpath(a1,a2,a3,a4,a5,a6,a7,a8,a9) result(path)
   !
   character(len=*), intent(in)           :: a1, a2
   character(len=*), intent(in), optional :: a3, a4, a5, a6, a7, a8, a9
   character(len=:), allocatable          :: path
   character(len=1)                       :: filesep

   filesep = separator()
   if(a1.ne.'')then
      path = trim(a1) // filesep // trim(a2)
   else
      path = trim(a2)
   endif
   if (present(a3)) path = path // filesep // trim(a3)
   if (present(a4)) path = path // filesep // trim(a4)
   if (present(a5)) path = path // filesep // trim(a5)
   if (present(a6)) path = path // filesep // trim(a6)
   if (present(a7)) path = path // filesep // trim(a7)
   if (present(a8)) path = path // filesep // trim(a8)
   if (present(a9)) path = path // filesep // trim(a9)
   path=adjustl(path//'  ')
   call substitute(path,filesep//filesep,filesep,start=2) ! some systems allow names starting with '//' or '\\'
   path=trim(path)
end function joinpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    separator(3f) - [M_io:ENVIRONMENT] try to determine pathname directory separator character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function separator() result(sep)
!!
!!     character(len=1) :: sep
!!
!!##DESCRIPTION
!!  Try to determine the separator character used to separate directory
!!  pathname components.
!!
!!  It is premised the separator is either a backslash or a slash character.
!!
!!  Once determined the value is cached for use with subsequent calls
!!
!!  It is assumed all processes use the same separator.
!!
!!  The tests use standard Fortran instead of ISO_C_BINDING routines. They
!!  can be very system dependent. If the queries fail the default returned
!!  is "/".

!!  First testing for the existence of "/.",  then if that fails a list
!!  of variable names assumed to contain directory paths {PATH|HOME} are
!!  examined first for a backslash, then a slash.  Assuming basically the
!!  choice is a ULS or MSWindows system, and users can do weird things like
!!  put a backslash in a ULS path and break it.
!!
!!   First, the environment variables PATH, HOME, PWD, and  SHELL  are
!!   examined for a backslash, then a slash.
!!
!!   Then, using the name the program was invoked with, then an INQUIRE(3f)
!!   of that name, then ".\NAME" and "./NAME" try to find an expected
!!   separator character.
!!
!!##EXAMPLE
!!
!!   sample usage
!!
!!    program demo_separator
!!    use M_io, only : separator
!!    implicit none
!!       write(*,*)'separator=',separator()
!!    end program demo_separator
!===================================================================================================================================
function separator() result(sep)
! use the pathname returned as arg0 to determine pathname separator
implicit none
integer                      :: ios
integer                      :: lun1, lun2
integer                      :: i
logical                      :: existing=.false.
character(len=1)             :: sep
!*!IFORT BUG:character(len=1),save        :: sep_cache=' '
integer,save                 :: isep=-1
character(len=4096)          :: name
character(len=:),allocatable :: envnames(:)
character(len=:),allocatable :: arg0
integer                      :: arg0_length
character(len=:),allocatable :: fname

    ! NOTE:  A parallel code might theoretically use multiple OS

    if(isep.ne.-1)then  ! use cached value.
        sep=char(isep)
        return
    endif

    FOUND: block
    ! simple, but does not work with ifort
    ! most MSWindows environments seem to work with backslash even when
    ! using POSIX filenames so do not rely on '\.'.
    inquire(file='/.',exist=existing,iostat=ios,name=name)
    if(existing.and.ios.eq.0)then
        sep='/'
        if(debug_m_os)then
           write(*,*)'METHOD 1:',sep
        else
           exit FOUND
        endif
    endif

    ! check variables names common to many platforms that usually have a directory path in them
    ! although a ULS file can contain a backslash and vice-versa (eg. "touch A\\B\\C"). Removed HOMEPATH because it
    ! returned a name with backslash on CygWin, Mingw, WLS even when using POSIX filenames in the environment.
    envnames=[character(len=10) :: 'PATH', 'HOME','PWD','SHELL']
    ! check variable for slash or backslash
    do i=1,size(envnames)
       if(index(get_env(envnames(i)),'\').ne.0)then
          sep='\'
          if(debug_m_os)then
             write(*,*)'METHOD 2A:',sep
          else
             exit FOUND
          endif
       elseif(index(get_env(envnames(i)),'/').ne.0)then
          sep='/'
          if(debug_m_os)then
             write(*,*)'METHOD 2B:',sep
          else
             exit FOUND
          endif
       endif
    enddo

   ! get argument name ARG0, although this may be just the command verb or nothing at all
   arg0_length=0
   name=' '
   call get_command_argument(0,length=arg0_length,status=ios)
   if(allocated(arg0))deallocate(arg0)
   allocate(character(len=arg0_length) :: arg0)
   call get_command_argument(0,arg0,status=ios)

   if(index(arg0,'\').ne.0)then
      sep='\'
      if(debug_m_os)then
         write(*,*)'METHOD 3A:',sep
      else
         exit FOUND
      endif
   elseif(index(arg0,'/').ne.0)then
      sep='/'
      if(debug_m_os)then
         write(*,*)'METHOD 3B:',sep
      else
         exit FOUND
      endif
   endif

   ! used to try './' and '.\' but exist test on some systems only returns true
   ! for a regular file so directory names always fail; although this can cause
   ! problems if trying to see if a filename is unused (the reverse is true in
   ! that you think a data file exists that is actually a directory!)

   ! try name returned by INQUIRE(3f) of arg0, as some PE will give canonical name
   existing=.false.
   name=' '
   inquire(file=arg0,iostat=ios,name=name)
   if(ios.eq.0)then
      if(index(name,'\').ne.0)then
         sep='\'
         if(debug_m_os)then
            write(*,*)'METHOD 4A:',sep
         else
            exit FOUND
         endif
      elseif(index(name,'/').ne.0)then
         write(*,*)'METHOD 4B:',sep
         sep='/'
         if(debug_m_os)then
            write(*,*)'METHOD 4C:',sep
         else
            exit FOUND
         endif
      endif
   endif

    ! could try opening a file assuming in a directory with write permission
    ! or can open /tmp/unique_file_name can be opened, which does on any Unix-Like System I know of

    ! used to call which(arg0) and see if could find pathname but now it calls this function

    ! used to then try to open "/tmp/UNIQUE_NAME" and assume "/" if successful, as any normal ULS has /tmp.
    ! but now some systems allow a "/" in a filename

    ! again, not all Fortran implementations return true if the file is not a regular file even if it exists
    inquire(file='///',iostat=ios,exist=existing) ! on POSIX systems this is the same as '/'.
    if(ios.eq.0)then
       if(existing)then
          sep='/'
          if(debug_m_os)then
             write(*,*)'METHOD 5:',sep
          else
             exit FOUND
          endif
       endif
    endif

    ! if can open file NAME but not file NAME? assume on MSWIndows as a last try
    ! as MSWindows pathnames cannot contain that character but ULS can
    fname=scratch()
    ! in case do not have write permission in the directory try a regular name first
    open(file=fname,newunit=lun1,iostat=ios)
    if(ios.eq.0)then
       open(file=fname//'?',newunit=lun2,iostat=ios)
       if(ios.ne.0)then
          sep='\'
          if(debug_m_os)then
             write(*,*)'METHOD 6:',sep
          else
             exit FOUND
          endif
       endif
    endif
    close(unit=lun1,iostat=ios,status='delete')
    close(unit=lun2,iostat=ios,status='delete')

    if(sep.eq.'')then
       write(*,*)'<WARNING>unknown system directory path separator'
       sep='/'
       if(debug_m_os)then
          write(*,*)'METHOD 7:',sep
       else
          exit FOUND
       endif
    endif
    endblock FOUND
    !*!IFORT BUG:sep_cache=sep
    isep=ichar(sep)
end function separator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!> Determine the OS type, maybe.
!!
!! Returns one of the integer values OS_UNKNOWN, OS_LINUX, OS_MACOS,
!! OS_WINDOWS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD.
!!
!! OS_UNKNOWN is returned if the operating system cannot be determined.
!!
!! If the file separator returned by separator() is "/" it is assumed
!! it is not a plain MSWindows system so tests are made for Linux, Unix,
!! Msys, Mingw and CygWin environments.
!!
!! When the file separator is returned as "/" the first check is that the
!! environment variable `OSTYPE` is compare with common names.  OSTYPE can
!! be exported from a bash(1) shell, but is not exported by default. The
!! user of a bash shell could be required to do an "export OSTYPE".
!! A check for the existence of files that can be found on
!! specific system types is tried.
!!
!! for situations like Cygwin/MSYS2/Git Bash on Windows there are options
!! where "/" and "\" might appear and the most important use of this
!! routine seems to be to determine the separator so using separator()
!! to separate basically into OS_WINDOWS and .not.OS_WINDOWS at this point
!!
!! If you just assume you have a POSIX system with uname(3f) use
!! M_system::system_uname(3f) and this is a lot easier
!!
!! if you can read a process or want to make a scratch file, running
!! "bash -c 'echo $OSTYPE'" would make a lot of sense.
!!
integer function get_os_type() result(r)
character(len=:),allocatable :: val
character(len=4096)          :: line
character(len=256)           :: message
integer                      :: lun
integer                      :: ios
integer                      :: i
type(string_t), allocatable  :: output(:)
    if(OS_CACHED .ne. -1)then
       r = OS_CACHED
       return
    else
       r = OS_UNKNOWN
    endif

    FOUND: block
       if(separator().eq.'/')then
           ! Check environment variable `OSTYPE`.
           val=lower(get_env('OSTYPE'))

           if(val.eq.'')then
              call read_cmd('uname -s',output)
              if(size(output).gt.0)then
                 val=val//lower(output(1)%s)
              endif
           endif

           ! WSL SAYS DOES NOT EXIST WITH GFORTRAN AND CANNOT READ FILE
           ! /proc/version is not a regular file and apparently some Fortran
           ! implementations will not read it
           if( val.eq.'' .and. exists('/proc/version'))then
              open(newunit=lun,iostat=ios,iomsg=message,file='/proc/version')
              if(ios.eq.0)then
                 line=' '
                 read(lun,'(a)',iostat=ios)line
                 if(ios.eq.0)then
                    val=trim(lower(line))
                 endif
              endif
           endif

           if(val.eq.'')then
           elseif (index(val, 'linux') > 0 )   then ; r = OS_LINUX   ! Linux
           elseif (index(val, 'mingw') > 0 )   then ; r = OS_LINUX   ! Linux (for now, anyway)
           elseif (index(val, 'darwin') > 0 )  then ; r = OS_MACOS   ! macOS
           elseif (index(val, 'cygwin') > 0 )  then ; r = OS_CYGWIN  ! Cygwin
           elseif (index(val, 'sunos') > 0 )   then ; r = OS_SOLARIS ! Solaris, OpenIndiana, ...
           elseif (index(val, 'solaris') > 0 ) then ; r = OS_SOLARIS
           elseif (index(val, 'freebsd') > 0 ) then ; r = OS_FREEBSD ! FreeBSD
           endif
           if(r.ne.OS_UNKNOWN) exit FOUND

           ! look for specific files that probably indicate an OS
           if (exists('/etc/os-release')) then ;         r = OS_LINUX   ! Linux
           elseif (exists('/usr/bin/sw_vers')) then;     r = OS_MACOS   ! macOS
           elseif (exists('/usr/bin/cygstart')) then;    r = OS_CYGWIN  ! Cygwin
           elseif (exists('/bin/freebsd-version')) then; r = OS_FREEBSD ! FreeBSD
           endif
       elseif(separator().eq.'\')then
          ! Check environment variable `OS`.
          val=lower(get_env('OS'))
           if (index(val, 'windows_nt') > 0 ) then
               r = OS_WINDOWS
               exit FOUND
           elseif (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then
           ! Windows, and maybe MSYS, MSYS2, MinGW, Git Bash
               r = OS_WINDOWS
               exit FOUND
           endif
           if(r.ne.OS_UNKNOWN) exit FOUND
       endif
    endblock FOUND

    OS_CACHED = r

end function get_os_type
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine run(cmd)
character(len=*), intent(in) :: cmd
integer                      :: exitstat
integer                      :: cmdstat
character(len=256)           :: cmdmsg
   write(*,'(*(a))') '+ ', cmd
   call execute_command_line(cmd, exitstat=exitstat,cmdstat=cmdstat,cmdmsg=cmdmsg)
   if (cmdstat /= 0) then
      print *, '<ERROR>*run*:Command failed:'//trim(cmdmsg)
   endif
end subroutine run
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!$BLOCK COMMENT -file which.3m_io.man
!>## NAME
!!    which(3f) - [M_io:ENVIRONMENT] given a command name find the pathname by searching
!!                the directories in the environment variable $PATH
!!    (LICENSE:PD)
!!
!!## SYNTAX
!!  function which(command) result(pathname)
!!
!!   character(len=*),intent(in)  :: command
!!   character(len=:),allocatable :: pathname
!!
!!## DESCRIPTION
!!   Given a command name find the first file with that name in the directories
!!   specified by the environment variable $PATH.
!!
!!## OPTIONS
!!   COMMAND   the command to search for
!!
!!## RETURNS
!!   PATHNAME  the first pathname found in the current user path. Returns blank
!!             if the command is not found.
!!
!!## EXAMPLE
!!  Sample program:
!!
!!    program demo_which
!!    use M_io, only : which
!!    implicit none
!!       write(*,*)'ls is ',which('ls')
!!       write(*,*)'dir is ',which('dir')
!!       write(*,*)'install is ',which('install')
!!    end program demo_which
!!
!!## AUTHOR
!!   John S. Urban
!!## LICENSE
!!   Public Domain
!$BLOCK
!===================================================================================================================================
function which(command) result(pathname)
character(len=*),intent(in)     :: command
character(len=:),allocatable    :: pathname, checkon, paths(:), exts(:)
integer                         :: i, j
   pathname=''
   call split(get_env('PATH'),paths,delimiters=merge(';',':',separator().eq.'\'))
   SEARCH: do i=1,size(paths)
      checkon=trim(joinpath(trim(paths(i)),command))
      select case(separator())
      case('/')
         if(exists(checkon))then
            pathname=checkon
            exit SEARCH
         endif
      case('\')
         if(exists(checkon))then
            pathname=checkon
            exit SEARCH
         endif
         call split(get_env('PATHEXT'),exts,delimiters=';')
         do j=1,size(exts)
            if(exists(checkon//'.'//trim(exts(j))))then
               pathname=checkon//'.'//trim(exts(j))
               exit SEARCH
            endif
         enddo
         if(exists(checkon//'.bat'))then
            pathname=checkon//'.bat'
            exit SEARCH
         endif
         if(exists(checkon//'.exe'))then
            pathname=checkon//'.exe'
            exit SEARCH
         endif
      end select
   enddo SEARCH
end function which
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine read_cmd(cmd,output)
character(len=*),intent(in)             :: cmd
type(string_t),allocatable,intent(out)  :: output(:)
integer                      :: ios
integer                      :: lun
character(len=:),allocatable :: scr
   if(allocated(output))deallocate(output) ! bug: INTENT(OUT) should be doing this automatically, IMO
   scr=scratch()
   call run(cmd//' > '//scr)
   open(newunit=lun,file=scr,iostat=ios)
   output=read_lines(lun)
   close(unit=lun, status="delete",iostat=ios)
end subroutine read_cmd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_OS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!  pure Fortran can use inquire and I/O routines, environment variables, and system commands and command arguments to
!  interact with the programming environment
!  o an external command could be shipped that returns an error code that would indicate system type
!  o on POSIX machines uname(3c) can easily be called, per M_system(3f). It can be searched for in the current path with which(3f)
!    assuming a command of the same name is not on a non-POSIX platform.
!  o Fortran does not seem reliable on testing if a file exists if it is not a plain file (ifort in particular)
!  o There are compiler extensions in major compilers that allow better routines, including a UNAME equivalent
!  o conditional code can be used that is system dependent.
!  o environment variables typical to particular systems can be inspected
!  o testing for the existence of platform-specific files can be used
!  o trying to open a scratch file with characters that are not allowed in filenames on particular systems may work, but
!    INQUIRE returns appear to vary between compilers, and there is no guarantee the pathname may fail for other reasons like
!    being in a read-only directory.
!  o names like ./. and .\. often do not work on an INQUIRE as some INQUIRES return EXIST as .FALSE. if the pathname is a directory
!  o currently the way fpm(1) is used, it is reasonable to assume a small scratch file can be made in the current directory
!    but something more robust using $TEMPNAM, $TEMP, $TMP or something else if preferred.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! The following reserved characters cannot be used in a Windows filename:
!
!     < (less than)
!     > (greater than)
!     : (colon)
!     " (double quote)
!     / (forward slash)
!     \ (backslash)
!     | (vertical bar or pipe)
!     ? (question mark)
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
