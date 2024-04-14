
module utils

	use iso_fortran_env
	implicit none

	double precision, parameter :: PI = 4.d0 * atan(1.d0)

	integer, parameter :: &
		EXIT_FAILURE = -1, &
		EXIT_SUCCESS = 0

	character, parameter :: &
			NULL_CHAR       = char( 0), &
			TAB             = char( 9), &
			LINE_FEED       = char(10), &
			VERT_TAB        = char(11), &
			CARRIAGE_RETURN = char(13), &
			ESC             = char(27)

	! TODO: make these variables, with colors disabled if output_unit is not tty
	! and an option to --force-color
	character(len = *), parameter :: &
			fg_bold               = esc//"[;1m", &
			fg_yellow             = esc//"[33m", &
			fg_bright_red         = esc//"[91m", &
			fg_bold_bright_red    = esc//"[91;1m", &
			fg_bold_bright_yellow = esc//"[93;1m", &
			fg_bright_green       = esc//"[92m", &
			fg_bright_yellow      = esc//"[93m", &
			fg_bright_blue        = esc//"[94m", &
			fg_bright_magenta     = esc//"[95m", &
			fg_bright_cyan        = esc//"[96m", &
			fg_bright_white       = esc//"[97m", &
			color_reset           = esc//"[0m"

	character(len = *), parameter :: &
		ERROR_STR = fg_bold_bright_red   //"Error"  //fg_bold//": "//color_reset, &
		WARN_STR  = fg_yellow//"Warning"//fg_bold//": "//color_reset

	!********

	interface to_str
		procedure :: to_str_i32
		procedure :: to_str_i64
	end interface to_str

	!********

	type str_t
		character(len = :), allocatable :: s
	end type str_t

	!********

	type str_builder_t
		! This is basically a dynamic char vector, but the type is a str and not
		! an actual array of single chars
		!
		! Syntran could benefit from this type, at least in its corresponding
		! read_line() fn
		character(len = :), allocatable :: s
		integer(kind = 8) :: len, cap
		contains
			procedure :: &
				push => push_str_builder, &
				trim => trim_str_builder
	end type str_builder_t

	!********

	type str_vec_t
		type(str_t), allocatable :: v(:)
		integer(kind = 8) :: len, cap
		contains
			procedure :: push => push_str
	end type str_vec_t

	!********

contains

!===============================================================================

function new_str_builder() result(sb)

	type(str_builder_t) :: sb

	sb%len = 0
	sb%cap = 16

	allocate(character(len = sb%cap) :: sb%s)

end function new_str_builder

!===============================================================================

function new_str_vec() result(vec)

	type(str_vec_t) :: vec

	vec%len = 0
	vec%cap = 2

	allocate(vec%v( vec%cap ))

end function new_str_vec

!===============================================================================

function trim_str_builder(sb) result(str)

	class(str_builder_t), intent(in) :: sb
	character(len = :), allocatable :: str

	str = sb%s(1: sb%len)

end function trim_str_builder

!===============================================================================

subroutine push_str_builder(sb, val)

	! Push one char at a time onto a str builder.  Could generalize to push a
	! whole primitive str of len > 1

	class(str_builder_t), intent(inout) :: sb
	character, intent(in) :: val

	!********

	character(len = :), allocatable :: tmp

	integer(kind = 8) :: i, tmp_cap

	!print *, "pushing """//val//""""

	sb%len = sb%len + 1
	if (sb%len > sb%cap) then
		!print *, 'growing str'

		! Grow the buffer capacity.  What is the optimal growth factor?
		tmp_cap = 2 * sb%cap
		allocate(character(len = tmp_cap) :: tmp)
		tmp(1: sb%cap) = sb%s

		call move_alloc(tmp, sb%s)
		sb%cap = tmp_cap

	end if
	sb%s(sb%len: sb%len) = val

end subroutine push_str_builder

!===============================================================================

subroutine push_str(vec, val)

	class(str_vec_t), intent(inout) :: vec

	character(len = *), intent(in) :: val

	!********

	type(str_t) :: val_str
	type(str_t), allocatable :: tmp(:)

	integer :: tmp_cap

	!print *, "pushing """//val//""""

	vec%len = vec%len + 1

	if (vec%len > vec%cap) then
		!print *, 'growing vec'

		tmp_cap = 2 * vec%len
		allocate(tmp( tmp_cap ))
		tmp(1: vec%cap) = vec%v

		call move_alloc(tmp, vec%v)
		vec%cap = tmp_cap

	end if

	val_str%s = val
	vec%v( vec%len ) = val_str

end subroutine push_str

!===============================================================================

function read_line(iu, iostat) result(str)

	! c.f. aoc-2022/utils.f90 and syntran/src/utils.f90
	!
	! This version reads WITHOUT backspacing, so it works on stdin too

	integer, intent(in) :: iu
	integer, optional, intent(out) :: iostat

	character(len = :), allocatable :: str

	!********

	character :: c
	character(len = :), allocatable :: tmp

	integer :: io

	type(str_builder_t) :: sb

	!print *, 'starting read_line()'

	! Read 1 character at a time until end
	sb = new_str_builder()
	do
		read(iu, '(a)', advance = 'no', iostat = io) c
		!print *, "c = """, c, """"

		if (io == iostat_end) exit
		if (io == iostat_eor) exit

		! In syntran, calling readln() one more time after the initial EOF
		! causes an infinite loop for some reason without this
		if (io /= 0) exit

		!if (c == carriage_return) exit
		!if (c == line_feed) exit

		call sb%push(c)

	end do
	str = sb%trim()

	!print *, "sb  = ", sb%s( 1: sb%len )
	!print *, "str = ", str

	if (io == iostat_end .or. io == iostat_eor) io = 0
	if (present(iostat)) iostat = io

end function read_line

!===============================================================================

function split(str, delims) result(strs)

	! This was translated from aoc-syntran and there are lots of off-by-one
	! differences going from syntran to fortran

	character(len = *), intent(in) :: str
	character(len = *), intent(in) :: delims

	!character(len = :), allocatable :: strs
	type(str_vec_t) :: strs

	!********

	integer :: i, i0, n, nout

	n = len(str)
	!print *, "n = ", n

	strs = new_str_vec()
	if (n == 0) return

	nout = 1
	if (scan(str(1:1), delims) > 0) nout = 0

	i = 1
	do while (i <= n)
		!print *, "i = ", i
		i0 = i

		i = scan  (str(i:n), delims) + i0 - 1
		if (i < i0) i = n + 1

		!print *, "i0, i = ", i0, i

		if (nout > 0) then
			call strs%push(str(i0: i - 1))
		end if

		i0 = i
		i = verify(str(i:n), delims) + i0 - 1
		if (i < i0) i = n + 1

		nout = nout + 1
	end do
	!print *, "nout = ", nout

end function split

!===============================================================================
subroutine unit_test_split()
	character(len = :), allocatable :: str
	integer :: i
	type(str_vec_t) :: strs
	str = "0,12,23,34,,7,,,,45,56,,10101,"
	strs = split(str, ",")
	!print *, "strs = ", strs%v(:)%s
	!print *, "strs = ", [(strs%v(i)%s, i = 1, strs%len)]
	print *, "strs = "//LINE_FEED//"["
	print "(a)", [(TAB//'"'//strs%v(i)%s//'",', i = 1, strs%len)]
	print "(a)", "]"
	call exit(0)
end subroutine unit_test_split
!===============================================================================

function to_str_i32(int_) result(str)
	integer(kind = 4), intent(in) :: int_
	character(len = :), allocatable :: str
	character :: buffer*16
	write(buffer, "(i0)") int_
	str = trim(buffer)
end function to_str_i32

!===============================================================================

function to_str_i64(int_) result(str)
	integer(kind = 8), intent(in) :: int_
	character(len = :), allocatable :: str
	character :: buffer*16
	write(buffer, "(i0)") int_
	str = trim(buffer)
end function to_str_i64

!===============================================================================

end module utils

!===============================================================================

