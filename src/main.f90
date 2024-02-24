
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

		! Input time bounds and step size
		double precision :: t_start, t_end, dt

		double precision  :: t   ! transient time value for current step
		integer(kind = 8) :: it  ! time index

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

	!********

	interface to_str
		procedure :: to_str_i32
		procedure :: to_str_i64
	end interface to_str

contains

!===============================================================================

function read_geom(filename) result(g)

	character(len = *), intent(in) :: filename

	type(geom_t) :: g

	!********

	character :: buf2*2
	character(len = 2) :: ca, cb, cc

	integer :: io, fid, iv, it, ibuf(9)

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

	! First pass: count vertices and triangles
	do
		read(fid, *, iostat = io) buf2
		if (io == iostat_end) exit
		call handle_read_io(filename, io)

		!print *, "buf2 = ", buf2

		if (buf2 == "v ") g%nv = g%nv + 1
		if (buf2 == "f ") g%nt = g%nt + 1

	end do
	rewind(fid)

	write(*,*) to_str(g%nv)//" vertices"
	write(*,*) to_str(g%nt)//" triangles"

	allocate(g%v( ND, g%nv ))
	allocate(g%t( NT, g%nt ))

	! Second pass: save data
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

			!! TODO: dynamically parse string for other obj face formats
			!read(fid, *, iostat = io) buf2, ibuf(1: 6)
			!read(fid, "(a,i0,x,i,x,i,x,i,x,i,x,i)", iostat = io) buf2, ibuf(1: 6)
			!read(fid, *, iostat = io) ca, ibuf(1), cb, ibuf(2), cc, ibuf(3)
			!read(fid, "(a,i12,x,i12,x,i12)", iostat = io) buf2, ibuf(1), cb, ibuf(2), cc, ibuf(3)

			!print *, "ibuf = ", ibuf
			!stop

			call handle_read_io(filename, io)

		end if

	end do
	!print "(3es16.6)", g%v
	!print "(3i12)", g%t
	print "(3i12)", g%t(:, 1: min(10, g%nt))

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
function to_str_i32(int_) result(str_)
	integer(kind = 4), intent(in) :: int_
	character(len = :), allocatable :: str_
	character :: buffer*16
	write(buffer, "(i0)") int_
	str_ = trim(buffer)
end function to_str_i32

!===============================================================================
function to_str_i64(int_) result(str_)
	integer(kind = 8), intent(in) :: int_
	character(len = :), allocatable :: str_
	character :: buffer*16
	write(buffer, "(i0)") int_
	str_ = trim(buffer)
end function to_str_i64
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
				write(*,*) ERROR_STR//"bad argument `"//argv//"`"
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
	character(kind=json_CK, len = :), allocatable :: key, sval, path

	integer :: ib
	integer(json_IK) :: ival, count_, count_gc, i, ic, igc, index_
	integer, allocatable :: template(:), t2(:,:)

	logical(json_LK) :: found
	logical, parameter :: STRICT = .false.  ! STRICT is not permissive

	type(json_core) :: json
	type(json_file) :: file_
	type(json_value), pointer :: p, proot, pw, pc, pp, pgc

	! initialize the class
	call file_%initialize()

	call file_%load(filename = filename)
	if (file_%failed()) then
		write(*,*) ERROR_STR//"could not load file """//filename//""""
		write(*,*) fg_bright_cyan
		call file_%print_error_message()
		write(*,*) color_reset
		call ribbit_exit(EXIT_FAILURE)
	end if

	call file_%print()

	call json%initialize()
	call file_%get(proot)

	do ic = 1, json%count(proot)
	call json%get_child(proot, ic, p)
	call json%info(p, name = key)

	select case (key)
	case ("world")
		!pw = p
		pw => p

	case default
		call bad_key(key, STRICT)

	end select
	end do
	!call json%get_child(proot, pw)

	! TODO: set world default (grav_accel, etc.)

	do ic = 1, json%count(pw)
	call json%get_child(pw, ic, p)
	call json%info(p, name = key)

	select case (key)
	case ("dt")
		call json%get(p, "@", w%dt)
		!print *, "w%dt = ", w%dt

	case ("t_start")
		call json%get(p, "@", w%t_start)
		!print *, "w%t_start = ", w%t_start

	case ("t_end")
		call json%get(p, "@", w%t_end)
		!print *, "w%t_end = ", w%t_end

	case ("grav_accel")

		w%grav_accel = get_array(json, p, ND)
		!print *, "grav_accel = ", w%grav_accel

	case ("bodies")
		count_ = json%count(p)
		!print *, "bodies count = ", count_
		allocate(w%bodies(count_))
		! TODO: set defaults for each body

		do ib = 1, count_
			call json%get_child(p, ib, pc)

			!print *, "traversing body ", ib

			! "grandchildren" count
			count_gc = json%count(pc)
			!print *, "count_gc = ", count_gc

			do igc = 1, count_gc
				call json%get_child(pc, igc, pgc)
				call json%info(pgc, name = key)

				select case (key)
				case ("geom")
					!call json%get(pgc, "@", w%geom_name)
					call json%get(pgc, "@", geom_name)
					w%bodies(ib)%geom = read_geom(geom_name)

				case ("pos")
					w%bodies(ib)%pos = get_array(json, pgc, ND)
					!print *, "w%bodies(ib)%pos", w%bodies(ib)%pos

				case ("vel")
					w%bodies(ib)%vel = get_array(json, pgc, ND)
					!print *, "w%bodies(ib)%vel", w%bodies(ib)%vel

				case ("coef_rest")
					call json%get(pgc, "@", w%bodies(ib)%coef_rest)

				case default
					call bad_key(key, permissive)
				end select

			end do
		end do

	case default
		call bad_key(key, permissive)

	end select
	end do

	if (json%failed()) then

		write(*,*) ERROR_STR//"could not load file """//filename//""""
		write(*,*) fg_bright_cyan
		call json%print_error_message()
		write(*,*) color_reset
		call ribbit_exit(EXIT_FAILURE)

	end if

	!call exit(0)

end function read_world

!===============================================================================

subroutine bad_key(key, permissive)
	character(len = *), intent(in) :: key
	logical, intent(in) :: permissive

	if (permissive) then
		write(*,*) WARN_STR //"bad json key """//key//""""
	else
		write(*,*) ERROR_STR//"bad json key """//key//""""
		call ribbit_exit(EXIT_FAILURE)
	end if

end subroutine bad_key

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

	!open(newunit = fid, file = "dump3.csv")
	open(newunit = fid, file = "dump4.csv", action = "write", iostat = io)
	! TODO: handle io

	w%t = w%t_start
	w%it = 0
	do while (w%t <= w%t_end)

		!print *, "t, z = ", w%t, w%bodies(1)%pos(3)
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
			b%pos = p0 + 0.5 * (v0 + b%vel) * w%dt
			if (b%pos(3) < w%ground_z) then
				b%pos = p0
				b%vel(3) =  -b%coef_rest * v0(3)
			end if

			w%bodies(ib) = b
		end do

		call write_step(w)
		w%it = w%it + 1
		w%t = w%t + w%dt
	end do
	call write_case(w)

	write(*,*) "number of time steps = ", w%it
	write(*,*) "ending ribbit_run()"

end subroutine ribbit_run

!===============================================================================

subroutine write_case(w)

	type(world_t), intent(in) :: w

	!********

	integer :: fid, io, i, ib

	open(newunit = fid, file = "scratch/ribbit-1-.case", &
		action = "write", iostat = io)

	write(fid, "(a)") "FORMAT"
	write(fid, "(a)") "type: ensight gold"
	write(fid, "(a)") "GEOMETRY"

	!!write(fid, "(a)") "model: 1 exgold2-**.geo"
	!write(fid, "(a)") "model: 1 exgold2-*.geo"
	write(fid, "(a)") "model: 1 "//"ribbit-1-"//"*.geo"

	!write(fid, "(a)") "VARIABLE"
	!write(fid, "(a)") "scalar per node: 1 Stress exgold2.scl**"
	!write(fid, "(a)") "vector per node: 1 Displacement exgold2.dis**"

	write(fid, "(a)") "TIME"
	write(fid, "(a)") "time set: 1"
	!write(fid, "(a)") "#number of steps: 2"
	!write(fid, "(a)") "number of steps: 3"
	write(fid, "(a)") "number of steps: "//to_str(w%it)

	write(fid, "(a)") "filename start number: 0"
	write(fid, "(a)") "filename increment: 1"
	write(fid, "(a)") "time values:"

	! TODO: write actual times
	do i = 1, w%it
		write(fid, *) i
	end do

	!write(fid, "(a)") "1.0"
	!write(fid, "(a)") "2.0"
	!write(fid, "(a)") "3.0"

	close(fid)

end subroutine write_case

!===============================================================================

subroutine write_step(w)

	type(world_t), intent(in) :: w

	!********

	integer :: fid, io, i, ib

	open(newunit = fid, file = "scratch/ribbit-1-"//to_str(w%it)//".geo", &
		action = "write", iostat = io)

	write(fid, "(a)") "ensight gold geometry file"
	write(fid, "(a)") "generated by ribbit"
	write(fid, "(a)") "node id off"
	write(fid, "(a)") "element id off"
	write(fid, "(a)") "part"
	write(fid, "(a)") "1"  ! TODO: body index?  1 part per body or cat into single part?
	write(fid, "(a)") "ribbit world part"
	write(fid, "(a)") "coordinates"

	!! TODO: cat all bodies into a single part
	ib = 1
	!write(fid, "(a)") "4"
	write(fid, "(i0)") w%bodies(ib)%geom%nv

	!!write(fid, "(a)") "# all x coords"
	!write(fid, "(a)") "0"
	!write(fid, "(a)") "1"
	!write(fid, "(a)") "0"
	!write(fid, "(a)") "0"

	! Translate by position

	! x coords
	write(fid, "(es16.6)") w%bodies(ib)%geom%v(1,:) + w%bodies(ib)%pos(1)

	! y coords
	write(fid, "(es16.6)") w%bodies(ib)%geom%v(2,:) + w%bodies(ib)%pos(2)

	! z coords
	write(fid, "(es16.6)") w%bodies(ib)%geom%v(3,:) + w%bodies(ib)%pos(3)

	write(fid, "(a)") "tria3"
	write(fid, "(i0)") w%bodies(ib)%geom%nt
	write(fid, "(3(i0,x))") w%bodies(ib)%geom%t

	!write(fid, "(a)") "1 2 3"
	!write(fid, "(a)") "1 2 4"
	!write(fid, "(a)") "1 3 4"
	!write(fid, "(a)") "2 3 4"

	close(fid)

end subroutine write_step

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

