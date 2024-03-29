
module ribbit

	use iso_fortran_env
	implicit none

	!********

	integer, parameter :: &
		RIBBIT_MAJOR = 0, &
		RIBBIT_MINOR = 1, &
		RIBBIT_PATCH = 0

	double precision, parameter :: PI = 4.d0 * atan(1.d0)

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

		double precision, allocatable :: v0(:,:)  ! original vertex locations
		double precision, allocatable :: v (:,:)  ! rotated/translated locations

		integer, allocatable          :: t(:,:)   ! triangles

	end type geom_t

	!********

	type matl_t
		double precision :: coef_rest, dens
		double precision :: friction_stat, friction_dyn
	end type matl_t

	!********

	type body_t

		type(geom_t) :: geom
		integer :: matl  ! index of world%matls(:) array

		double precision :: pos(ND)
		double precision :: vel(ND)

		double precision :: rot(ND, ND)
		double precision :: ang_vel(ND)

		double precision :: vol, mass
		double precision :: inertia(ND, ND)

	end type body_t

	!********

	type world_t

		double precision :: grav_accel(ND)

		type(body_t), allocatable :: bodies(:)
		type(matl_t), allocatable :: matls (:)

		double precision :: ground_pos(ND)
		double precision :: ground_nrm(ND)

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
	write(*,*) "reading geometry file """//filename//""""

	g%nv = 0
	g%nt = 0

	open(newunit = fid, file = filename, action = "read", iostat = io)
	call handle_open_read_io(filename, io)

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
	!print "(3i12)", g%t(:, 1: min(10, g%nt))

end function read_geom

!===============================================================================

function cross(a, b) result(c)

	double precision, intent(in) :: a(ND), b(ND)
	double precision :: c(ND)

	c(1) = a(2) * b(3) - a(3) * b(2)
	c(2) = a(3) * b(1) - a(1) * b(3)
	c(3) = a(1) * b(2) - a(2) * b(1)

end function cross

!===============================================================================

subroutine get_inertia(b, w)

	! Get mass, center of mass, volume, and inertia tensor

	type(body_t), intent(inout) :: b
	type(world_t), intent(in)   :: w

	!********

	double precision :: vol_tet, com_tet(ND), vol, com(ND)
	double precision :: v(ND, 4), x(4), y(4), z(4)
	double precision :: ixx, iyy, izz, ixy, iyz, izx

	integer :: i, j, k

	vol = 0.d0
	com = 0.d0
	do i = 1, b%geom%nt

		! 6 * volume of a tetrahedron formed by triangle i and origin
		vol_tet = dot_product(cross(    &
			b%geom%v(:, b%geom%t(1,i)), &
			b%geom%v(:, b%geom%t(2,i))), &
			b%geom%v(:, b%geom%t(3,i)))

		vol = vol + vol_tet

		! Center of mass of this tetrahedron
		com_tet = 0.25d0 * sum(b%geom%v(:, b%geom%t(:,i)), 2)

		! Overall center of mass is weighted average
		com = com + vol_tet * com_tet

	end do
	com = com / vol
	vol = vol / 6.d0

	b%vol = vol

	! Now that the overall center of mass is known, do another loop to get the
	! inertia tensor (relative to overall com)
	b%inertia = 0.d0
	do i = 1, b%geom%nt

		! Repeat volume calculation.  Could be saved in array.  Time/space
		! tradeoff
		vol_tet = dot_product(cross(    &
			b%geom%v(:, b%geom%t(1,i)), &
			b%geom%v(:, b%geom%t(2,i))), &
			b%geom%v(:, b%geom%t(3,i)))
		vol_tet = vol_tet / 6.d0

		! TODO: get rid of v temp var.  Also move this outer loop *after*
		! translating all verts by com and loop up to 3 instead of 4
		v(:, 1:3) = b%geom%v(:, b%geom%t(:,i))
		v(:,   4) = 0.d0

		x = v(1,:) - com(1)
		y = v(2,:) - com(2)
		z = v(3,:) - com(3)

		! Ref:  https://thescipub.com/pdf/jmssp.2005.8.11.pdf
		ixx = 0.d0
		iyy = 0.d0
		izz = 0.d0
		do k = 1, 4
		do j = 1, k
			ixx = ixx + y(k) * y(j) + z(k) * z(j)
			iyy = iyy + z(k) * z(j) + x(k) * x(j)
			izz = izz + x(k) * x(j) + y(k) * y(j)
		end do
		end do
		ixx = vol_tet * ixx / 10
		iyy = vol_tet * iyy / 10
		izz = vol_tet * izz / 10

		iyz = 0.d0
		izx = 0.d0
		ixy = 0.d0
		do k = 1, 4
			iyz = iyz + y(k) * z(k)
			izx = izx + z(k) * x(k)
			ixy = ixy + x(k) * y(k)
			do j = 1, 4
				iyz = iyz + y(k) * z(j)
				izx = izx + z(k) * x(j)
				ixy = ixy + x(k) * y(j)
			end do
		end do
		iyz = vol_tet * iyz / 20
		izx = vol_tet * izx / 20
		ixy = vol_tet * ixy / 20

		!print *, "ixx = ", ixx
		!print *, "iyy = ", iyy
		!print *, "izz = ", izz
		!print *, "iyz = ", iyz
		!print *, "izx = ", izx
		!print *, "ixy = ", ixy

		! Components are arranged in inertia tensor like this:
		!
		! [  ixx, -ixy, -izx ]
		! [ -ixy,  iyy, -iyz ]
		! [ -izx, -iyz,  izz ]

		b%inertia(1,1) = b%inertia(1,1) + ixx
		b%inertia(2,2) = b%inertia(2,2) + iyy
		b%inertia(3,3) = b%inertia(3,3) + izz

		b%inertia(1,2) = b%inertia(1,2) - ixy
		b%inertia(2,3) = b%inertia(2,3) - iyz
		b%inertia(3,1) = b%inertia(3,1) - izx

	end do
	b%inertia(2,1) = b%inertia(1,2)
	b%inertia(3,2) = b%inertia(2,3)
	b%inertia(1,3) = b%inertia(3,1)

	b%inertia = b%inertia * w%matls(b%matl)%dens
	print *, "b%inertia = "
	print "(3es16.6)", b%inertia

	! Translate all vertices so that the center of mass is at the origin
	do i = 1, b%geom%nv
		b%geom%v(:,i) = b%geom%v(:,i) - com
	end do

	! Backup original vertex locations so that rounding errors do not warp the
	! shape of the body
	b%geom%v0 = b%geom%v

	b%mass = w%matls(b%matl)%dens * b%vol

	print *, "vol  = ", b%vol
	print *, "com  = ", com
	print *, "mass = ", b%mass

	! TODO: log bounding box summary

end subroutine get_inertia

!===============================================================================

subroutine handle_read_io(filename, io)
	character(len = *), intent(in) :: filename
	integer, intent(in) :: io
	if (io /= 0) call panic("cannot read file """//filename//"""")
end subroutine handle_read_io

!===============================================================================

subroutine handle_open_write_io(filename, io)
	character(len = *), intent(in) :: filename
	integer, intent(in) :: io
	if (io /= 0) call panic("cannot open file """//filename//""" for writing")
end subroutine handle_open_write_io

!===============================================================================

subroutine handle_open_read_io(filename, io)
	character(len = *), intent(in) :: filename
	integer, intent(in) :: io
	if (io /= 0) call panic("cannot open file """//filename//""" for reading")
end subroutine handle_open_read_io

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
		call panic("missing required argument after """//argv0//"""")
	end if

	call get_command_argument(i, buffer, status = io)
	if (io == STAT_TRUNC) then
		! Could make buffer allocatable and automatically try resizing
		call panic("command argument too long after """//argv0//"""")

	else if (io /= EXIT_SUCCESS) then
		call panic("cannot get command argument after """//argv0//"""")

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
		to_str(RIBBIT_MAJOR)//"."// &
		to_str(RIBBIT_MINOR)//"."// &
		to_str(RIBBIT_PATCH)

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

	double precision :: ang(ND)

	integer :: ib, im
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

	! Set world defaults
	w%grav_accel = 0.d0
	w%ground_pos = 0.d0
	w%ground_nrm = [0, 0, 1]
	w%t_start = 0.d0
	w%t_end = 1.d0
	w%dt = 0.1d0

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

	case ("ground_pos")
		w%ground_pos = get_array(json, p, ND)
	case ("ground_nrm")
		w%ground_nrm = get_array(json, p, ND)
		call normalize_s(w%ground_nrm)

	case ("bodies")
		count_ = json%count(p)
		!print *, "bodies count = ", count_
		allocate(w%bodies(count_))

		do ib = 1, count_

			! Set defaults for each body
			w%bodies(ib)%pos = 0.d0
			w%bodies(ib)%vel = 0.d0
			w%bodies(ib)%ang_vel = 0.d0
			w%bodies(ib)%matl = 1
			!w%bodies(ib)%geom  ! TODO: check has_geom for each body
			ang = 0.d0

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
					call json%get(pgc, "@", geom_name)
					w%bodies(ib)%geom = read_geom(geom_name)

				case ("pos")
					w%bodies(ib)%pos = get_array(json, pgc, ND)
					!print *, "w%bodies(ib)%pos", w%bodies(ib)%pos

				case ("vel")
					w%bodies(ib)%vel = get_array(json, pgc, ND)
					!print *, "w%bodies(ib)%vel", w%bodies(ib)%vel

				case ("ang")
					ang = get_array(json, pgc, ND)

					! Convert from degrees to radians, and then to rotation matrix
					!
					! TODO: initialize rot by default ang above
					w%bodies(ib)%rot = get_rot(PI / 180.d0 * ang)

				case ("ang_vel")
					w%bodies(ib)%ang_vel = get_array(json, pgc, ND)
					!print *, "w%bodies(ib)%ang_vel", w%bodies(ib)%ang_vel

					! Convert from degrees to radians
					w%bodies(ib)%ang_vel = PI / 180.d0 * w%bodies(ib)%ang_vel

				case ("matl")
					call json%get(pgc, "@", w%bodies(ib)%matl)
					!print *, "matl = ", w%bodies(ib)%matl

				case default
					call bad_key(key, permissive)
				end select

			end do
		end do

	case ("matls")
		count_ = json%count(p)
		allocate(w%matls(count_))

		do im = 1, count_

			! Set defaults for each material
			w%matls(im)%coef_rest = 0.9d0
			w%matls(im)%dens      = 1000.d0
			w%matls(im)%friction_stat = 0.15d0
			w%matls(im)%friction_dyn  = 0.1d0

			call json%get_child(p, im, pc)

			! "grandchildren" count
			count_gc = json%count(pc)
			!print *, "count_gc = ", count_gc

			do igc = 1, count_gc
				call json%get_child(pc, igc, pgc)
				call json%info(pgc, name = key)

				select case (key)
				case ("coef_rest")
					call json%get(pgc, "@", w%matls(im)%coef_rest)

				case ("friction_stat")
					call json%get(pgc, "@", w%matls(im)%friction_stat)

				case ("friction_dyn")
					call json%get(pgc, "@", w%matls(im)%friction_dyn)

				case ("dens")
					call json%get(pgc, "@", w%matls(im)%dens)

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

end function read_world

!===============================================================================

subroutine init_world(w)

	type(world_t), intent(inout) :: w

	!********

	integer :: ib

	! TODO: define 1 matl prop if not otherwise defined

	do ib = 1, size(w%bodies)
		call get_inertia(w%bodies(ib), w)
	end do

end subroutine init_world

!===============================================================================

subroutine bad_key(key, permissive)
	character(len = *), intent(in) :: key
	logical, intent(in) :: permissive

	if (permissive) then
		write(*,*) WARN_STR //"bad json key """//key//""""
	else
		call panic("bad json key """//key//"""")
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
	integer :: i, count_
	type(json_value), pointer :: pc

	count_ = json%count(p)
	if (count_ /= n) then
		call json%get_path(p, path)
		call panic("array at json path """//path &
			//""" must have "//to_str(n)//" components")
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

	character(len = : ), allocatable :: csv_file

	integer :: ib, io
	integer :: fid

	write(*,*) "starting ribbit_run()"

	csv_file = "dump4.csv"
	open(newunit = fid, file = csv_file, action = "write", iostat = io)
	call handle_open_write_io(csv_file, io)

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
			call update_body(w, w%bodies(ib), ib)
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

subroutine update_body(w, b, ib)

	type(world_t), intent(in) :: w
	type(body_t), intent(inout) :: b
	integer, intent(in) :: ib  ! don't check for collisions with self

	!********

	double precision, parameter :: tol = 0.001

	double precision :: p0(ND), v0(ND), rot0(ND, ND), i1_r1_nrm(ND), &
		vr(ND), vp1(ND), vp2(ND), r1(ND), nrm(ND), m1, i1(ND, ND), &
		jr(ND), jr_mag, e, i1_lu(ND, ND), tng(ND), fe(ND), js, jd_mag, &
		jf(ND), jf_mag, i1_r1_tng(ND), jf_max

	integer :: i, ncolliding
	integer, allocatable :: ipiv(:)

	logical :: colliding

	v0 = b%vel
	b%vel = v0 + w%grav_accel * w%dt

	p0   = b%pos
	rot0 = b%rot

	b%pos = p0 + 0.5 * (v0 + b%vel) * w%dt

	! Update rotations by multiplying by a rotation matrix, not by
	! adding vector3's!
	b%rot = matmul(get_rot(b%ang_vel * w%dt), b%rot)

	! Rounding errors might accumulate after many time steps.
	! Re-orthonormalize just in case
	call gram_schmidt(b%rot)

	call update_pose(b)

	! In case of collision along an entire face or edge, get the centroid of
	! that face/edge instead of just one vertex, by taking the average of all
	! colliding vertices
	colliding = .false.
	ncolliding = 0
	r1 = 0.d0
	do i = 1, b%geom%nv
		if (dot_product(w%ground_nrm, b%geom%v(:,i) - w%ground_pos) <= 0) then
			colliding = .true.
			ncolliding = ncolliding + 1
			r1 = r1 + b%geom%v(:,i) - b%pos
		end if
	end do
	r1 = r1 / ncolliding

	if (colliding) then

		! Collide body (body 1) with ground (body 2).  The ground has
		! infinite mass and inertia, so many terms become zero

		! Velocities of each bodies' points *at point of contact* (not
		! center of mass)
		vp2 = 0
		vp1 = b%vel + cross(b%ang_vel, r1)
		!vp1 = v0 + cross(b%ang_vel, r1)

		! Relative velocity
		vr = vp2 - vp1

		! Normal vector of collision plane.  I think this minus sign
		! doesn't make a difference
		nrm = -w%ground_nrm

		! Mass and inertia *in world frame of reference*
		m1 = b%mass
		i1 = matmul(matmul(b%rot, b%inertia), transpose(b%rot))

		! Coefficient of restitution.  TODO: average/min for two bodies
		e = w%matls(b%matl)%coef_rest

		i1_r1_nrm = invmul(i1, cross(r1, nrm))

		! Normal impulse magnitude.  Ref: https://en.wikipedia.org/wiki/Collision_response
		jr_mag = -(1.d0 + e) * dot_product(vr, nrm) / &
			(1.d0/m1 + dot_product(nrm, cross(i1_r1_nrm, r1)))
		!print *, "jr_mag = ", jr_mag

		! Sum of external forces acting on body
		fe = m1 * w%grav_accel
		!print *, "fe = ", fe

		! Tangent vector
		if (abs(dot_product(normalize(vr), nrm)) > tol) then
			tng = vr - dot_product(vr, nrm) * nrm
			call normalize_s(tng)
		else if (abs(dot_product(normalize(fe), nrm)) > tol) then
			tng = fe - dot_product(fe, nrm) * nrm
			call normalize_s(tng)
		else
			tng = 0
		end if
		!tng = -tng
		!print *, "tng = ", tng

		!! Get the friction (tangential) impulse vector `jf`.  Ref: wiki ibid
		!js = w%matls(b%matl)%friction_stat * abs(jr_mag)
		!if (m1 * dot_product(vr, tng) <= js) then
		!	!jf = -m1 * dot_product(vr, tng) * tng
		!	jf_mag = -m1 * dot_product(vr, tng)
		!else
		!	jd_mag = w%matls(b%matl)%friction_dyn * abs(jr_mag)
		!	!jf = -jd_mag * tng
		!	jf_mag = -jd_mag
		!end if

		! TODO: this is the same matrix inverted, but multiplied by a different
		! RHS vector.  Go back to two-phase factor and solve
		i1_r1_tng = invmul(i1, cross(r1, tng))

		!! Ref:  https://gafferongames.com/post/collision_response_and_coulomb_friction/

		!jf_mag = -(1.d0 + e) * dot_product(vr, tng) / &
		!jf_mag = -e * dot_product(vr, tng) / &
		jf_mag = -dot_product(vr, tng) / &
			(1.d0/m1 + dot_product(tng, cross(i1_r1_tng, r1)))

		! Apply friction cone clamp.  TODO: when should this be static friction?
		jf_max = w%matls(b%matl)%friction_dyn * abs(jr_mag)
		jf_mag = max(min(jf_mag, jf_max), -jf_max)

		!print *, "jf_mag = ", jf_mag

		!b%vel = v0 - jr_mag / m1 * nrm - jf / m1
		b%vel = v0 - jr_mag / m1 * nrm - jf_mag / m1 * tng

		b%ang_vel = b%ang_vel - jr_mag * i1_r1_nrm - jf_mag * i1_r1_tng

		!print *, "b%ang_vel", b%ang_vel

		b%pos = p0
		b%rot = rot0

		call update_pose(b)

		!print *, ""

	end if

end subroutine update_body

!===============================================================================

!subroutine lu_factor(a, ipiv)
!
!	! Get the LU factorization of matrix A
!
!	! TODO: pack a and ipiv into an lu_t struct
!	double precision, intent(inout) :: a(:, :)
!	integer, allocatable, intent(out) :: ipiv(:)
!
!	!********
!
!	integer :: io, n, m, lda
!
!	!print *, "a = ", a
!	!print *, "b = ", b
!
!	! In general, these sizes could have different values if b is a matrix or A
!	! is not square
!
!	n = size(a, 2)
!
!	m   = n
!	lda = n
!
!	allocate(ipiv(n))
!
!	!a_ = a
!	!call dgetrf(n, nrhs, a_, lda, ipiv, x, ldb, io)
!	call dgetrf(m, n, a, lda, ipiv, io)
!
!	if (io /= 0) call panic("lapack error in dgetrf()")
!
!	!print *, "x = ", x
!
!end subroutine lu_factor
!
!!===============================================================================
!
!function lu_solve(a, ipiv, b) result(x)
!	! TODO: rename a -> lu
!
!	! Solve the matrix equation:
!	!
!	!     A * x = b
!	!
!	! for x:
!	!
!	!     x = inv(A) * b
!	!
!	! where `a` is already LU factorized by lu_factor().
!
!	double precision, intent(in) :: a(:, :), b(:)
!	double precision, allocatable :: x(:)
!	integer, allocatable, intent(in) :: ipiv(:)
!
!	!********
!
!	!double precision, allocatable :: a_(:,:)
!
!	integer :: io, n, nrhs, lda, ldb
!
!	!print *, "a = ", a
!	!print *, "b = ", b
!
!	! In general, these sizes could have different values if b is a matrix or A
!	! is not square
!
!	n = size(a, 2)
!
!	nrhs = 1
!	lda = n
!	ldb = n
!
!	!allocate(ipiv(n))
!
!	x = b
!	!a_ = a
!	!call dgesv(n, nrhs, a_, lda, ipiv, x, ldb, io)
!	call dgetrs("N", n, nrhs, a, lda, ipiv, x, ldb, io)
!
!	if (io /= 0) call panic("lapack error in dgetrs()")
!
!	!print *, "x = ", x
!
!end function lu_solve

!===============================================================================

function invmul(a, b) result(x)

	! Solve the matrix equation:
	!
	!     A * x = b
	!
	! for x:
	!
	!     x = inv(A) * b

	double precision, intent(in) :: a(:, :), b(:)
	double precision, allocatable :: x(:)

	!********

	double precision, allocatable :: a_(:,:)

	integer :: io, n, nrhs, lda, ldb
	integer, allocatable :: ipiv(:)

	!print *, "a = ", a
	!print *, "b = ", b

	! In general, these sizes could have different values if b is a matrix or A
	! is not square

	n = size(a, 2)

	nrhs = 1
	lda = n
	ldb = n

	allocate(ipiv(n))

	x = b
	a_ = a
	call dgesv(n, nrhs, a_, lda, ipiv, x, ldb, io)

	if (io /= 0) call panic("lapack error in dgesv()")

	!print *, "x = ", x

end function invmul

!===============================================================================

subroutine gram_schmidt(a)

	! Could be generalized to higher dimensions

	double precision, intent(inout) :: a(ND, ND)

	a(:,2) = a(:,2) - project(a(:,2), a(:,1))
	a(:,3) = a(:,3) - project(a(:,3), a(:,1)) - project(a(:,3), a(:,2))

end subroutine gram_schmidt

!===============================================================================

function normalize(v) result(u)
	double precision, intent(in) :: v(:)
	double precision, allocatable :: u(:)
	double precision :: norm_
	norm_ = norm2(v)
	! TODO: DRY with subroutine version normalize_s
	if (norm_ > 1.d-12) then
		u = v / norm_
	else
		u = 0
	endif
end function normalize

!===============================================================================

subroutine normalize_s(v)
	double precision, intent(inout) :: v(:)
	double precision :: norm_
	norm_ = norm2(v)
	if (norm_ > 1.d-12) then
		v = v / norm_
	else
		v = 0
	endif
end subroutine normalize_s

!===============================================================================

function project(v, u) result(w)
	double precision, intent(in) :: v(:), u(:)
	double precision, allocatable :: w(:)

	! Project v onto u
	w = dot_product(v, u) / dot_product(u, u) * u

end function project

!===============================================================================

function get_rot(ang) result(rot)

	! Convert a vector3 ang to a rotation matrix rot

	double precision, intent(in) :: ang(ND)
	double precision :: rot(ND, ND)

	!********

	double precision :: ax, ay, az, &
		rotx(ND, ND), roty(ND, ND), rotz(ND, ND)

	! Rotation angles about each axis
	ax = ang(1)
	ay = ang(2)
	az = ang(3)

	! Cardinal rotation matrices

	rotx(:,1) = [1.d0,     0.d0,    0.d0]
	rotx(:,2) = [0.d0,  cos(ax), sin(ax)]
	rotx(:,3) = [0.d0, -sin(ax), cos(ax)]

	roty(:,1) = [cos(ay), 0.d0, -sin(ay)]
	roty(:,2) = [   0.d0, 1.d0,     0.d0]
	roty(:,3) = [sin(ay), 0.d0,  cos(ay)]

	rotz(:,1) = [ cos(az), sin(az), 0.d0]
	rotz(:,2) = [-sin(az), cos(az), 0.d0]
	rotz(:,3) = [    0.d0,    0.d0, 1.d0]

	! First rotate about x, then about y, then about z
	rot = matmul(matmul(rotz, roty), rotx)
	!rot = matmul(rotz, matmul(roty, rotx))
	!rot = matmul(rotx, matmul(roty, rotz))

	!print *, "rot "
	!print "(3es16.6)", rot

end function get_rot

!===============================================================================

subroutine update_pose(b)

	type(body_t), intent(inout)  :: b

	!********

	integer :: i

	! Rotate first and then translate by com position

	!print *, "rotating"

	! TODO: lapack dgemm().  This crashes on large data (e.g. homer.obj) when
	! compiled without "-heap-arrays0" ifx option
	b%geom%v = matmul(b%rot, b%geom%v0)

	!print *, "translating"
	b%geom%v(1,:) = b%geom%v(1,:) + b%pos(1)
	b%geom%v(2,:) = b%geom%v(2,:) + b%pos(2)
	b%geom%v(3,:) = b%geom%v(3,:) + b%pos(3)

	!print *, "done"

end subroutine update_pose

!===============================================================================

subroutine write_case(w)
	! Ref: https://dav.lbl.gov/archive/NERSC/Software/ensight/doc/Manuals/UserManual.pdf

	type(world_t), intent(in) :: w

	!********

	character(len = :), allocatable :: case_file

	integer :: fid, io, i

	! TODO: add arg for filename.  Encapsulate in world struct?
	!
	! TODO: mkdir.  wrap C fn?
	case_file = "scratch/ribbit-1.case"

	open(newunit = fid, file = case_file, action = "write", iostat = io)
	call handle_open_write_io(case_file, io)

	write(fid, "(a)") "FORMAT"
	write(fid, "(a)") "type: ensight gold"
	write(fid, "(a)") "GEOMETRY"

	write(fid, "(a)") "model: 1 "//"ribbit-1-"//"*.geo"

	write(fid, "(a)") "TIME"
	write(fid, "(a)") "time set: 1"
	write(fid, "(a)") "number of steps: "//to_str(w%it)

	write(fid, "(a)") "filename start number: 0"
	write(fid, "(a)") "filename increment: 1"
	write(fid, "(a)") "time values:"

	! TODO: write actual times
	write(fid, "(es16.6)") [(dble(i), i = 1, w%it)]

	close(fid)
	write(*,*) fg_bright_magenta//"finished writing file """// &
		case_file//""""//color_reset

end subroutine write_case

!===============================================================================

subroutine write_c80(fid, str_)

	integer, intent(in) :: fid
	character(len = *), intent(in) :: str_

	!********

	character :: buf80*80

	buf80 = str_
	buf80(len(buf80): len(buf80)) = LINE_FEED
	write(fid) buf80

end subroutine write_c80

!===============================================================================

subroutine write_step(w)
	! Ref: https://dav.lbl.gov/archive/NERSC/Software/ensight/doc/Manuals/UserManual.pdf

	type(world_t), intent(in) :: w

	!********

	character(len = :), allocatable :: geom_file

	integer :: fid, io, i, ib

	real, allocatable :: r(:,:)

	! TODO: add json (or cmd) binary toggle option
	logical, parameter :: binary = .true.

	!print *, "starting write_step()"

	if (binary) then

		! TODO: add arg for filename.  Encapsulate in world struct?
		!
		! TODO: mkdir.  wrap C fn?
		geom_file = "scratch/ribbit-1-"//to_str(w%it)//".geo"
		!open(newunit = fid, form = "unformatted", access = "stream", &
		open(newunit = fid, access = "stream", &
			file = geom_file, action = "write", iostat = io)
		call handle_open_write_io(geom_file, io)

		call write_c80(fid, "Fortran Binary")

		call write_c80(fid, "ensight gold geometry file")
		call write_c80(fid, "generated by ribbit")
		call write_c80(fid, "node id off")
		call write_c80(fid, "element id off")

		do ib = 1, size(w%bodies)

			call write_c80(fid, "part")
			write(fid) ib
			call write_c80(fid, "ribbit world part")
			call write_c80(fid, "coordinates")

			write(fid) w%bodies(ib)%geom%nv

			! Copy and convert vertex positions to 4-byte real
			r = w%bodies(ib)%geom%v

			write(fid) r(1,:)
			write(fid) r(2,:)
			write(fid) r(3,:)

			call write_c80(fid, "tria3")
			write(fid) w%bodies(ib)%geom%nt
			write(fid) w%bodies(ib)%geom%t

		end do

		close(fid)

	else

		! TODO: add arg for filename.  Encapsulate in world struct?
		!
		! TODO: mkdir.  wrap C fn?
		geom_file = "scratch/ribbit-1-"//to_str(w%it)//".geo"
		open(newunit = fid, file = geom_file, action = "write", iostat = io)
		call handle_open_write_io(geom_file, io)

		write(fid, "(a)") "ensight gold geometry file"
		write(fid, "(a)") "generated by ribbit"
		write(fid, "(a)") "node id off"
		write(fid, "(a)") "element id off"

		do ib = 1, size(w%bodies)

			write(fid, "(a)") "part"
			write(fid, "(i0)") ib
			write(fid, "(a)") "ribbit world part"
			write(fid, "(a)") "coordinates"

			write(fid, "(i0)") w%bodies(ib)%geom%nv

			! Copy and convert vertex positions to 4-byte real
			r = w%bodies(ib)%geom%v

			write(fid, "(es16.6)") r(1,:)
			write(fid, "(es16.6)") r(2,:)
			write(fid, "(es16.6)") r(3,:)

			write(fid, "(a)") "tria3"
			write(fid, "(i0)") w%bodies(ib)%geom%nt
			write(fid, "(3(i0,x))") w%bodies(ib)%geom%t  ! whitespace matters.  3 numbers per line

		end do

		close(fid)

	end if

	!print *, "ending write_step()"

end subroutine write_step

!===============================================================================

subroutine panic(msg)
	character(len = *), intent(in) :: msg
	write(*,*) ERROR_STR//msg
	call ribbit_exit(EXIT_FAILURE)
end subroutine panic

!===============================================================================

subroutine ribbit_exit(exit_code)
	integer, intent(in) :: exit_code
	if (exit_code == EXIT_SUCCESS) write(*,*) fg_bright_green//"finished ribbit"//color_reset
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
	call init_world(world)

	call ribbit_run(world)
	call ribbit_exit(EXIT_SUCCESS)

end program main

!===============================================================================

