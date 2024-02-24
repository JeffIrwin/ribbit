
module ribbit

	use iso_fortran_env
	implicit none

	!********

	integer, parameter :: &
		ribbit_major = 0, &
		ribbit_minor = 1, &
		ribbit_patch = 0

	integer, parameter :: ND = 3, NT = 3

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

	type geom_t

		! Number of vertices/triangles
		integer :: nv, nt

		double precision, allocatable :: v(:,:)  ! vertices
		integer, allocatable          :: t(:,:)  ! triangles

	end type geom_t

	!********

	type body_t

		type(geom_t) :: geom

		double precision :: pos(ND)
		double precision :: vel(ND)

		! TODO: refactor into material struct
		double precision :: coef_rest

	end type body_t

	!********

	type world_t

		double precision :: grav_accel(ND)

		type(body_t), allocatable :: bodies(:)

		double precision :: ground_z

		double precision :: t
		double precision :: t_start, t_end, dt

	end type world_t

	!********

	type args_t

		character(len = :), allocatable :: ribbit_file

		logical :: &
			ribbit_file_arg = .false., &
			permissive      = .false., &
			version         = .false., &
			help            = .false.

	end type args_t

contains

!===============================================================================

function read_geom(filename) result(g)

	character(len = *), intent(in) :: filename

	type(geom_t) :: g

	!********

	character :: buf2*2

	integer :: io, fid, iv, it

	! Could be extended to switch on different file formats based on the
	! filename extension
	write(*,*) "Reading geometry file """//filename//""""

	g%nv = 0
	g%nt = 0

	open(newunit = fid, file = filename, action = "read", iostat = io)
	if (io /= 0) then
		write(*,*) ERROR_STR//"cannot open geometry file """//filename//""""
		call ribbit_exit(EXIT_FAILURE)
	end if

	do
		read(fid, *, iostat = io) buf2
		if (io == iostat_end) exit
		call handle_read_io(filename, io)

		!print *, "buf2 = ", buf2

		if (buf2 == "v ") g%nv = g%nv + 1
		if (buf2 == "f ") g%nt = g%nt + 1

	end do
	rewind(fid)

	print *, "nv = ", g%nv
	print *, "nt = ", g%nt

	allocate(g%v( ND, g%nv ))
	allocate(g%t( NT, g%nt ))

	iv = 0
	it = 0
	do
		read(fid, *, iostat = io) buf2
		if (io == iostat_end) exit
		call handle_read_io(filename, io)

		!print *, "buf2 = ", buf2

		if (buf2 == "v ") then
			iv = iv + 1
			backspace(fid)
			read(fid, *, iostat = io) buf2, g%v(:,iv)
			call handle_read_io(filename, io)

		else if (buf2 == "f ") then
			it = it + 1
			backspace(fid)
			read(fid, *, iostat = io) buf2, g%t(:,it)
			call handle_read_io(filename, io)

		end if

	end do
	print "(3es16.6)", g%v
	print "(3i12)", g%t

	!stop  ! TODO

end function read_geom

!===============================================================================

subroutine handle_read_io(filename, io)
	character(len = *), intent(in) :: filename
	integer, intent(in) :: io
	if (io /= 0) then
		write(*,*) ERROR_STR//"cannot read file """//filename//""""
		call ribbit_exit(EXIT_FAILURE)
	end if
end subroutine handle_read_io

!===============================================================================

subroutine get_next_arg(i, argv)
	integer, intent(inout) :: i
	character(len = :), allocatable, intent(out) :: argv
	!********
	character(len = :), allocatable, save :: argv0
	character(len = 1024) :: buffer
	integer, parameter :: STAT_TRUNC = -1
	integer :: io, argc
	logical, save :: first = .true.

	if (first) then
		first = .false.
		call get_command_argument(0, buffer)
		argv0 = trim(buffer)
	end if

	i = i + 1
	argc = command_argument_count()
	if (i > argc) then
		write(*,*) ERROR_STR//"missing required argument after """//argv0//""""
		call ribbit_exit(EXIT_FAILURE)
	end if

	call get_command_argument(i, buffer, status = io)
	if (io == STAT_TRUNC) then
		! Could make buffer allocatable and automatically try resizing
		write(*,*) ERROR_STR//"command argument too long after """//argv0//""""
		call ribbit_exit(EXIT_FAILURE)

	else if (io /= EXIT_SUCCESS) then
		write(*,*) ERROR_STR//"cannot get command argument after """//argv0//""""
		call ribbit_exit(EXIT_FAILURE)

	end if
	argv = trim(buffer)
	!print *, "argv = ", argv

	argv0 = argv

end subroutine get_next_arg

!===============================================================================
function to_str(int_) result(str_)
	integer, intent(in) :: int_
	character(len = :), allocatable :: str_
	character :: buffer*16
	write(buffer, *) int_
	str_ = trim(buffer)
end function to_str
!===============================================================================

function read_args() result(args)

	! This argument parser is based on http://docopt.org/
	!
	! c.f. github.com/jeffirwin/cali and syntran

	type(args_t) :: args

	!********

	character(len = :), allocatable :: argv, str_, url, version

	integer :: i, io, argc, ipos

	logical :: error = .false.

	!! Defaults
	!args%maxerr = maxerr_def

	argc = command_argument_count()
	!print *, "argc = ", argc

	i = 0
	ipos = 0
	do while (i < argc)
		call get_next_arg(i, argv)

		select case (argv)
		case ("-h", "--help", "-help")
			args%help    = .true.

		case ("-p", "--permissive")
			args%permissive = .true.

		!case ("--fmax-errors")
		!	call get_next_arg(i, str_)
		!	read(str_, *, iostat = io) args%maxerr
		!	if (io /= exit_success) then
		!		write(*,*) ERROR_STR//"--fmax-errors "//str_ &
		!			//" is not a valid integer"
		!		error = .true.
		!	end if

		case ("--version")
			args%version = .true.

		case default

			! Positional arg
			ipos = ipos + 1

			if (ipos == 1) then
				args%ribbit_file_arg = .true.
				args%ribbit_file = argv

			!else if (ipos == 2) then
			!	args%lout_file = .true.
			!	args%out_file  = argv

			else
				write(*,*) ERROR_STR//"unknown argument `"//argv//"`"
				error = .true.

			end if

		end select

	end do

	if (ipos < 1 .and. .not. (args%help .or. args%version)) then
		write(*,*) ERROR_STR//"ribbit file not defined"
		error = .true.
	end if

	url = "https://github.com/JeffIrwin/ribbit"

	version = &
		to_str(ribbit_major)//"."// &
		to_str(ribbit_minor)//"."// &
		to_str(ribbit_patch)

	if (.not. error) then
		write(*,*)
		write(*,*) fg_bright_magenta//"ribbit "//version//color_reset
		write(*,*) fg_bright_magenta//url//color_reset
		write(*,*)
	end if

	if (error .or. args%help) then

		write(*,*) fg_bold//"Usage:"//color_reset
		write(*,*) "	ribbit <file.ribbit>"
		write(*,*) "	ribbit -h | --help"
		write(*,*) "	ribbit --version"
		write(*,*)
		write(*,*) fg_bold//"Options:"//color_reset
		write(*,*) "	-h --help           Show this help"
		write(*,*) "	--version           Show version"
		write(*,*) "	-p --permissive     Use loose json schema rules"
		write(*,*)

		if (.not. args%help) call ribbit_exit(EXIT_FAILURE)
	end if

	!if (.not. args%lout_file) then
	!	if (args%waterfall) then
	!		args%out_file = "./build/waterfall-"//basename(args%ttf_file)//".ppm"
	!	else
	!		args%out_file = "./build/"//basename(args%ttf_file)//".ppm"
	!	end if
	!end if

end function read_args

!===============================================================================

function read_world(filename, permissive) result(w)
	use json_module

	character(len = *), intent(in) :: filename
	logical, intent(in) :: permissive

	type(world_t) :: w

	!********

	character(len = :), allocatable :: geom_name
	character(kind=json_CK, len=:), allocatable :: key, sval, path

	integer :: ib
	integer(json_IK) :: ival, count_, count_w, count_gc, i, ic, igc, index_
	integer, allocatable :: template(:), t2(:,:)

	logical(json_LK) :: found

	type(json_core) :: core
	type(json_file) :: json
	type(json_value), pointer :: p, pw, pc, pp, pgc

	! initialize the class
	call json%initialize()

	call json%load(filename = filename)
	if (json%failed()) then
		write(*,*) ERROR_STR//"could not load file """//filename//""""
		write(*,*) fg_bright_cyan
		call json%print_error_message()
		write(*,*) color_reset
		call ribbit_exit(EXIT_FAILURE)
	end if

	call json%print()

	call core%initialize()
	call json%get(pw)
	!call json%get(pw, "world")
	!call json%get_by_path(pw, "world")
	call core%get_child(pw, pw)
	!! TODO: traverse root to check typos, name of "world"

	count_w = core%count(pw)
	!print *, "count world children = ", count_w

	! TODO: set world default (grav_accel, etc.)

	do ic = 1, count_w
	call core%get_child(pw, ic, p)
	call core%info(p, name = key)

	select case (key)
	case ("dt")
		call core%get(p, "@", w%dt)
		!print *, "w%dt = ", w%dt

	case ("t_start")
		call core%get(p, "@", w%t_start)
		!print *, "w%t_start = ", w%t_start

	case ("t_end")
		call core%get(p, "@", w%t_end)
		!print *, "w%t_end = ", w%t_end

	case ("grav_accel")

		w%grav_accel = get_array(core, p, ND)
		!print *, "grav_accel = ", w%grav_accel

	case ("bodies")
		count_ = core%count(p)
		!print *, "bodies count = ", count_
		allocate(w%bodies(count_))
		! TODO: set defaults for each body

		do ib = 1, count_
			call core%get_child(p, ib, pc)

			!print *, "traversing body ", ib

			! "grandchildren" count
			count_gc = core%count(pc)
			!print *, "count_gc = ", count_gc

			do igc = 1, count_gc
				call core%get_child(pc, igc, pgc)
				call core%info(pgc, name = key)

				select case (key)
				case ("geom")
					!call core%get(pgc, "@", w%geom_name)
					call core%get(pgc, "@", geom_name)
					w%bodies(ib)%geom = read_geom(geom_name)

				case ("pos")
					w%bodies(ib)%pos = get_array(core, pgc, ND)
					!print *, "w%bodies(ib)%pos", w%bodies(ib)%pos

				case ("vel")
					w%bodies(ib)%vel = get_array(core, pgc, ND)
					!print *, "w%bodies(ib)%vel", w%bodies(ib)%vel

				case ("coef_rest")
					call core%get(pgc, "@", w%bodies(ib)%coef_rest)

				case default
					if (permissive) then
						write(*,*) WARN_STR //"unknown json key """//key//""""
					else
						write(*,*) ERROR_STR//"unknown json key """//key//""""
						call ribbit_exit(EXIT_FAILURE)
					end if

				end select

			end do
		end do

	case default

		if (permissive) then
			write(*,*) WARN_STR //"unknown json key """//key//""""
		else
			write(*,*) ERROR_STR//"unknown json key """//key//""""
			call ribbit_exit(EXIT_FAILURE)
		end if

	end select
	end do

	if (core%failed()) then

		write(*,*) ERROR_STR//"could not load file """//filename//""""
		write(*,*) fg_bright_cyan
		call core%print_error_message()
		write(*,*) color_reset
		call ribbit_exit(EXIT_FAILURE)

	end if

	!call exit(0)

end function read_world

!===============================================================================

function get_array(json, p, n) result(array)
	use json_module

	type(json_core) :: json
	type(json_value), pointer, intent(in) :: p
	integer, intent(in) :: n

	double precision :: array(n)

	!********

	character(len = :), allocatable :: path
	integer :: i, io, count_
	type(json_value), pointer :: pc

	count_ = json%count(p)
	if (count_ /= n) then
		call json%get_path(p, path)
		write(*,*) ERROR_STR//"array at json path """//path &
			//""" must have 3 components"
		call ribbit_exit(EXIT_FAILURE)
	end if

	do i = 1, count_
		call json%get_child(p, i, pc)
		call json%get(pc, "@", array(i))
	end do

end function get_array

!===============================================================================

subroutine ribbit_run(w)

	use json_module
	type(world_t), intent(inout) :: w

	!********

	double precision :: p0(ND), v0(ND)

	integer :: ib, i, io
	integer :: fid

	logical :: found

	type(body_t)  :: b

	write(*,*) "starting ribbit_run()"

	open(newunit = fid, file = "dump3.csv")
	w%t = w%t_start
	do while (w%t <= w%t_end)

		print *, "t, z = ", w%t, w%bodies(1)%pos(3)
		write(fid, "(es16.6)", advance = "no") w%t
		do ib = 1, size(w%bodies)
			write(fid, "(3es16.6)", advance = "no") w%bodies(ib)%pos
		end do
		write(fid, *)

		do ib = 1, size(w%bodies)
			b = w%bodies(ib)

			v0 = b%vel
			b%vel = v0 + w%grav_accel * w%dt

			p0 = b%pos
			b%pos = p0 + 0.5 * (v0 + b%vel)
			if (b%pos(3) < w%ground_z) then
				b%pos = p0
				b%vel(3) =  -b%coef_rest * v0(3)
			end if

			w%bodies(ib) = b
		end do

		w%t = w%t + w%dt
	end do

	write(*,*) "ending ribbit_run()"

end subroutine ribbit_run

!===============================================================================

subroutine ribbit_exit(exit_code)
	integer, intent(in) :: exit_code
	call exit(exit_code)
end subroutine ribbit_exit

!===============================================================================

end module ribbit

!===============================================================================

program main
	use ribbit
	implicit none

	type(args_t)  :: args
	type(world_t) :: world

	args  = read_args()
	world = read_world(args%ribbit_file, args%permissive)

	call ribbit_run(world)
	call ribbit_exit(EXIT_SUCCESS)

end program main

