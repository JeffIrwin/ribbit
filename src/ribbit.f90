
module ribbit

	use iso_fortran_env
	use utils

	implicit none

	!********

	integer, parameter :: &
		RIBBIT_MAJOR = 0, &
		RIBBIT_MINOR = 1, &
		RIBBIT_PATCH = 0

	integer, parameter :: ND = 3, NT = 3

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

		! previous state
		double precision :: pos0(ND), vel0(ND), rot0(ND, ND)

	end type body_t

	!********

	type world_t

		double precision :: grav_accel(ND)

		type(body_t), allocatable :: bodies(:)
		type(matl_t), allocatable :: matls (:)

		double precision :: ground_pos(ND)
		double precision :: ground_nrm(ND)
		logical :: has_ground

		! Input time bounds and step size
		double precision :: t_start, t_end, dt

		double precision  :: t   ! transient time value for current step
		integer(kind = 8) :: it  ! time index

	end type world_t

	!********

	! External C fns
	integer, external :: &
		del_file, &
		make_dir

	!********

contains

!===============================================================================

function read_geom(filename) result(g)

	character(len = *), intent(in) :: filename

	type(geom_t) :: g

	!********

	character :: buf2*2
	character(len = :), allocatable :: str

	integer :: i, io, fid, iv, it, step

	type(str_vec_t) :: strs

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

		if (buf2 == "v " .or. buf2 == "v"//TAB) g%nv = g%nv + 1
		if (buf2 == "f " .or. buf2 == "f"//TAB) g%nt = g%nt + 1

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
		read(fid, "(a)", iostat = io, advance = "no") buf2
		if (io == iostat_end) exit
		if (io == iostat_eor) cycle
		call handle_read_io(filename, io)

		!print *, "buf2 = """, buf2, """"

		if (buf2 == "v " .or. buf2 == "v"//TAB) then
			iv = iv + 1
			backspace(fid)
			read(fid, *, iostat = io) buf2, g%v(:,iv)
			call handle_read_io(filename, io)

		else if (buf2 == "f " .or. buf2 == "f"//TAB) then
			it = it + 1
			backspace(fid)

			! Dynamically parse str for any obj face format.  There is
			! optional data on obj for texture and normal coordinates

			str = read_line(fid, io)
			call handle_read_io(filename, io)
			!print *, "str = """//str//""""

			strs = split(str, "f/ "//TAB)
			!print *, "strs%len = ", strs%len

			if (.not. any(strs%len == [3, 6, 9])) then
				call panic("bad face format.  Expected 3, 6, or 9 " &
					//"numbers but got "//to_str(strs%len))
			end if

			! OBJ faces may have 3, 6, or 9 numbers.  We only care about the
			! vertex indices
			step = strs%len / 3

			do i = 0, 2
				read(strs%v(i * step + 1)%s, *, iostat = io) g%t(i+1, it)
				call handle_read_io(filename, io)
			end do
			!print *, "t = ", g%t(:,it)

		else if (buf2(1:1) == "#" .or. buf2 == "") then
			! Skip comment
			read(fid, *, iostat = io) buf2
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
	write(*,*) "b%inertia = "
	write(*, "(3es16.6)") b%inertia

	! Translate all vertices so that the center of mass is at the origin
	do i = 1, b%geom%nv
		b%geom%v(:,i) = b%geom%v(:,i) - com
	end do

	! Backup original vertex locations so that rounding errors do not warp the
	! shape of the body
	b%geom%v0 = b%geom%v

	b%mass = w%matls(b%matl)%dens * b%vol

	write(*,*) "vol  = ", b%vol
	write(*,*) "com  = ", com
	write(*,*) "mass = ", b%mass

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

function read_world(filename, permissive) result(w)

	use json_module

	character(len = *), intent(in) :: filename
	logical, intent(in) :: permissive

	type(world_t) :: w

	!********

	character(len = :), allocatable :: geom_name
	character(kind=json_CK, len = :), allocatable :: key

	double precision :: ang(ND), default_coef_rest, default_dens, &
		default_friction_stat, default_friction_dyn

	integer :: ib, im
	integer(json_IK) :: count_, count_gc, ic, igc

	logical, parameter :: STRICT = .false.  ! STRICT is not permissive
	logical :: has_geom

	type(json_core) :: json
	type(json_file) :: file_
	type(json_value), pointer :: p, proot, pw, pc, pgc

	write(*,*) "Reading file """//filename//""""

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
		pw => p

	case default
		call bad_key(key, STRICT)

	end select
	end do

	! Set defaults for each material
	default_coef_rest = 0.9d0
	default_dens      = 1000.d0
	default_friction_stat = 0.15d0
	default_friction_dyn  = 0.1d0

	! Set world defaults
	w%grav_accel = 0.d0
	w%ground_pos = 0.d0
	w%ground_nrm = [0, 0, 1]
	w%has_ground = .false.
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
		w%has_ground = .true.
		w%ground_pos = get_array(json, p, ND)
	case ("ground_nrm")
		w%has_ground = .true.
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
			has_geom = .false.
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
					has_geom = .true.
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

			if (.not. has_geom) then
				call panic("body "//to_str(ib)//" does not have a ""geom"" defined")
			end if

			! Convert from degrees to radians, and then to rotation matrix
			w%bodies(ib)%rot = get_rot(PI / 180.d0 * ang)

		end do

	case ("matls")
		count_ = json%count(p)
		allocate(w%matls(count_))

		do im = 1, count_

			! Set defaults for each material
			w%matls(im)%coef_rest     = default_coef_rest
			w%matls(im)%dens          = default_dens
			w%matls(im)%friction_stat = default_friction_stat
			w%matls(im)%friction_dyn  = default_friction_dyn

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

	if (.not. allocated(w%matls)) then
		allocate(w%matls(1))
		w%matls(1)%coef_rest     = default_coef_rest
		w%matls(1)%dens          = default_dens
		w%matls(1)%friction_stat = default_friction_stat
		w%matls(1)%friction_dyn  = default_friction_dyn
	end if

end function read_world

!===============================================================================

subroutine init_world(w)

	! Consider inlining this at the end of read_world()

	type(world_t), intent(inout) :: w

	!********

	integer :: ib

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

subroutine tri_line(a, b, c, e, f, p, stat)

	! Given a triangle formed by points `a`, `b`, and `c`, and a line segment
	! formed by points `e` and `f`, find their intersection point `p` or whether
	! there is no intersection
	!
	! `stat` is 0 if a valid intersection exists
	!
	! For a sketch of this geometric problem, see ribbit/doc/tri-line.png
	!
	! c.f. https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm

	double precision, intent(in) :: a(ND), b(ND), c(ND), e(ND), f(ND)

	double precision, intent(out) :: p(ND)
	integer, intent(out) :: stat

	!********

	double precision :: t, bu, bv, bw
	double precision :: u(ND), v(ND), ef(ND), params(4)
	double precision :: mat(ND, ND), rhs(ND, 1)
	double precision, allocatable :: lhs(:,:)
	double precision :: line_tol

	integer :: i, io

	! Vectors along the triangle's edges
	u = b - a
	v = c - a

	! Vector along line segment
	ef = f - e

	! Represent intersection point p in the triangle:
	!
	!     p = a + bu * u + bv * v
	!
	! Represent the point p on the line segment:
	!
	!     p = e + t * ef
	!
	! Eliminate p:
	!
	!     p = a + bu * u + bv * v
	!     p = e + t * ef
	!
	!     a + bu * u + bv * v = e + t * ef
	!
	! Rearrange:
	!
	!     u * bu + v * bv - ef * t  = e - a
	!
	! These are 3 equations with 3 unknowns (bu, bv, and t):
	!
	! mat * [bu, bv, t] = e - a

	mat(:, 1) = u
	mat(:, 2) = v
	mat(:, 3) = -ef

	rhs(:, 1) = e - a

	! Do the linear algebra
	lhs = invmul(mat, rhs, io)
	if (io /= 0) then
		!write(*,*) WARN_STR//"line segment and triangle are parallel"
		stat = -2
		return
	end if

	! Unpack the answers from lhs
	bu = lhs(1, 1)  ! barycentric coordinate in tri
	bv = lhs(2, 1)  ! barycentric coordinate
	t  = lhs(3, 1)  ! parametric coordinate along line segment from e to f

	bw = 1.d0 - bu - bv   ! 3rd, non-independent barycentric coordinate

	!! All of these parametric coordinates need to be in the range [0, 1] for the intersection to be valid
	!print *, "t  = ", t
	!print *, "bu = ", bu
	!print *, "bv = ", bv
	!print *, "bw = ", bw

	params = [t, bu, bv, bw]
	stat = 0

	! Skip p coordinate calculation if intersection is invalid.  Could return
	! different status codes if virtual intersection is outside of line segment
	! vs outside of triangle

	! Buffer the lines with a 1% extension but *not* the triangles.  This makes
	! the collision detection a little more robust against body a-to-b vs body
	! b-to-a order problem, e.g. for "inputs/space-cubes.ribbit".

	line_tol = 0.01d0
	!line_tol = 0.001d0

	if (t < -line_tol .or. t > 1.d0 + line_tol) then
		stat = -1
		return
	end if

	if (any(params(2:4) < 0)) then
		stat = -1
		return
	end if

	if (any(params(2:4) > 1)) then
		stat = -1
		return
	end if

	p = e + clamp01(t) * ef

end subroutine tri_line

!===============================================================================

double precision function clamp01(x)
	double precision, intent(in) :: x
	clamp01 = clamp(x, 0.d0, 1.d0)
end function clamp01

!===============================================================================

double precision function clamp(x, min_, max_)
	double precision, intent(in) :: x, min_, max_
	clamp = max(min(x, max_), min_)
end function clamp

!===============================================================================

subroutine ribbit_run(w)

	use json_module
	type(world_t), intent(inout) :: w

	!********

	character(len = : ), allocatable :: csv_file

	integer :: ia, ib, io
	integer :: fid

	logical, parameter :: dump_csv = .false.

	write(*,*) "starting ribbit_run()"

	if (dump_csv) then
		csv_file = "dump4.csv"
		open(newunit = fid, file = csv_file, action = "write", iostat = io)
		call handle_open_write_io(csv_file, io)
	end if

	w%t = w%t_start
	w%it = 0
	do while (w%t <= w%t_end)

		!print *, "t, z = ", w%t, w%bodies(1)%pos(3)
		!print *, "w%it = ", w%it

		if (dump_csv) then
			write(fid, "(es16.6)", advance = "no") w%t
			do ib = 1, size(w%bodies)
				write(fid, "(3es16.6)", advance = "no") w%bodies(ib)%pos
			end do
			write(fid, *)
		end if

		do ib = 1, size(w%bodies)
			call cache_body(w%bodies(ib))
		end do

		do ib = 1, size(w%bodies)
			call apply_grav(w, w%bodies(ib))
		end do

		do ib = 1, size(w%bodies)
			call collide_ground(w, w%bodies(ib))
		end do

		do ib = 1, size(w%bodies)
			do ia = 1, size(w%bodies)
			!do ia = 1, ib - 1
			!do ia = ib + 1, size(w%bodies)
				if (ia == ib) cycle
				call collide_bodies(w, w%bodies(ia), w%bodies(ib))
				!call collide_bodies(w, w%bodies(ib), w%bodies(ia))
			end do
		end do

		call write_step(w)
		w%it = w%it + 1
		w%t  = w%t  + w%dt
	end do
	call write_case(w)

	write(*,*) "number of time steps = ", w%it
	write(*,*) "ending ribbit_run()"

end subroutine ribbit_run

!===============================================================================

subroutine cache_body(b)
	! Save initial state
	type(body_t), intent(inout) :: b

	b%pos0 = b%pos
	b%rot0 = b%rot

	b%vel0 = b%vel

end subroutine cache_body

!===============================================================================

subroutine apply_grav(w, b)

	! Update body `b` due to gravitational acceleration.  Could be renamed to
	! something like `apply_forces()` if generalized to other things

	type(world_t), intent(inout) :: w
	type(body_t), intent(inout) :: b

	!********

	double precision, parameter :: tol = 0.001  ! coincident vector angle-ish tol

	double precision :: rot0(ND, ND), i1_r1_nrm(ND), &
		vr(ND), vp1(ND), vp2(ND), r1(ND), nrm(ND), m1, i1(ND, ND), &
		jr_mag, e, tng(ND), fe(ND), jf_mag, i1_r1_tng(ND), jf_max

	double precision :: rhs(ND,2)
	double precision, allocatable :: tmp(:,:)

	integer :: i, ncolliding, ia

	logical :: colliding

	b%vel = b%vel0 + w%grav_accel * w%dt

	b%pos = b%pos0 + 0.5 * (b%vel0 + b%vel) * w%dt

	! Update rotations by multiplying by a rotation matrix, not by
	! adding vec3's!  Rotations are not related to gravity, but we have to set
	! the pose before calling expensive update_vertices()
	b%rot = matmul(get_rot(b%ang_vel * w%dt), b%rot)

	! Rounding errors might accumulate after many time steps.
	! Re-orthonormalize just in case
	call gram_schmidt(b%rot)

	call update_vertices(b)

end subroutine apply_grav

!===============================================================================

subroutine collide_ground(w, b)

	type(world_t), intent(in) :: w
	type(body_t), intent(inout) :: b

	!********

	double precision, parameter :: tol = 0.001  ! coincident vector angle-ish tol

	double precision :: i1_r1_nrm(ND), &
		vr(ND), vp1(ND), vp2(ND), r1(ND), nrm(ND), m1, i1(ND, ND), &
		jr_mag, e, tng(ND), fe(ND), jf_mag, i1_r1_tng(ND), jf_max

	double precision :: rhs(ND,2)
	double precision, allocatable :: tmp(:,:)

	integer :: i, ncolliding, ia

	logical :: colliding

	!********
	! Update due to collision with the global ground, which is like a special
	! symbolic body with infinite inertia and extents

	! In case of collision along an entire face or edge, get the centroid of
	! that face/edge instead of just one vertex, by taking the average of all
	! colliding vertices
	colliding = .false.
	ncolliding = 0
	r1 = 0.d0
	do i = 1, b%geom%nv
		! TODO: change `has_ground` check into an early return after refactoring
		if (w%has_ground .and. dot_product(w%ground_nrm, b%geom%v(:,i) - w%ground_pos) <= 0) then
			colliding = .true.
			ncolliding = ncolliding + 1
			r1 = r1 + b%geom%v(:,i) - b%pos
		end if
	end do
	r1 = r1 / ncolliding

	! TODO: early return
	if (colliding) then
		!print *, "ncolliding = ", ncolliding

		! Collide body (body 1) with ground (body 2).  The ground has
		! infinite mass and inertia, so many terms become zero

		! Velocities of each bodies' points *at point of contact* (not
		! center of mass)
		vp2 = 0
		vp1 = b%vel + cross(b%ang_vel, r1)

		! Relative velocity
		vr = vp2 - vp1

		! Normal vector of collision plane.  I think this minus sign
		! doesn't make a difference
		nrm = -w%ground_nrm

		! Mass and inertia *in world frame of reference*
		m1 = b%mass
		i1 = matmul(matmul(b%rot, b%inertia), transpose(b%rot))

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

		! Pack data into a matrix for lapack
		rhs(:,1) = cross(r1, nrm)
		rhs(:,2) = cross(r1, tng)

		tmp = invmul(i1, rhs)

		! Unpack
		i1_r1_nrm = tmp(:,1)
		i1_r1_tng = tmp(:,2)

		! Coefficient of restitution
		e = w%matls(b%matl)%coef_rest

		! Normal impulse magnitude.  Ref: https://en.wikipedia.org/wiki/Collision_response
		jr_mag = -(1.d0 + e) * dot_product(vr, nrm) / &
			(1.d0/m1 + dot_product(nrm, cross(i1_r1_nrm, r1)))
		!print *, "jr_mag = ", jr_mag

		! Ref:  https://gafferongames.com/post/collision_response_and_coulomb_friction/

		jf_mag = -e * dot_product(vr, tng) / &
		!jf_mag = -(1.d0 + e) * dot_product(vr, tng) / &
		!jf_mag = -dot_product(vr, tng) / &
			(1.d0/m1 + dot_product(tng, cross(i1_r1_tng, r1)))

		! Apply friction cone clamp.  TODO: when should this be static friction?
		jf_max = w%matls(b%matl)%friction_dyn * abs(jr_mag)
		jf_mag = clamp(jf_mag, -jf_max, jf_max)
		!print *, "jf_mag = ", jf_mag

		! Average velocity to add a little damping
		b%vel = 0.5d0 * (b%vel0 + b%vel) - jr_mag * nrm / m1 - jf_mag * tng / m1
		!b%vel = b%vel0 - jr_mag * nrm / m1 - jf_mag * tng / m1

		b%ang_vel = b%ang_vel - jr_mag * i1_r1_nrm - jf_mag * i1_r1_tng

		!print *, "b%ang_vel", b%ang_vel

		!b%pos = b%pos0
		!b%rot = b%rot0
		b%pos = 0.5d0 * (b%pos + b%pos0)
		b%rot = 0.5d0 * (b%rot + b%rot0)
		!b%pos = 0.3d0 * b%pos + 0.7d0 * b%pos0
		!b%rot = 0.3d0 * b%rot + 0.7d0 * b%rot0

		call update_vertices(b)

		!print *, ""

	end if

end subroutine collide_ground

!===============================================================================

subroutine collide_bodies(w, a, b)

	! Check if any edge of body `a` collides with any face of body `b`.  If so,
	! update their outgoing velocity states

	type(world_t), intent(in) :: w
	type(body_t), intent(inout) :: a, b

	!********

	double precision, parameter :: tol = 0.001  ! coincident vector angle-ish tol

	double precision :: va1(ND), va2(ND), vb1(ND), vb2(ND), vb3(ND), p(ND), &
		r(ND), r1(ND), r2(ND), nrm(ND), vp1(ND), vp2(ND), vr(ND), m1, m2, &
		i1(ND,ND), i2(ND,ND), fe1(ND), fe2(ND), tng(ND), i1_r1_nrm(ND), &
		i1_r1_tng(ND), i2_r2_nrm(ND), i2_r2_tng(ND), e, jr_mag, jf_mag, &
		friction_dyn, jf_max, nrm_(ND)
	double precision :: rhs(ND,2)
	double precision, allocatable :: tmp(:,:)

	integer :: stat, ita, ie, iva1, iva2, itb, ivb1, ivb2, ivb3, nr

	!print *, "starting collide_bodies()"
	!print *, "a%pos = ", a%pos
	!print *, "b%pos = ", b%pos
	!print *, ""

	! TODO: return early if bbox check passes.  Need to set and cache bbox first
	! (in update_vertices()?)

	! Iterate through each edge of body `a` and get the average position `r` and
	! normal `nrm` of the collision point in the global coordinate system
	r   = 0.d0
	nrm = 0.d0
	nr  = 0
	do ita = 1, a%geom%nt
	do ie  = 1, NT

		iva1 = a%geom%t(    ie       , ita)
		iva2 = a%geom%t(mod(ie,3) + 1, ita)
		if (iva1 > iva2) cycle  ! only do each edge once
		!print *, "iva* = ", iva1, iva2

		va1 = a%geom%v(:, iva1)
		va2 = a%geom%v(:, iva2)
		!print *, "va1 = ", va1
		!print *, "va2 = ", va2

		! Iterate through each triangle of body b
		do itb = 1, b%geom%nt

			ivb1 = b%geom%t(1, itb)
			ivb2 = b%geom%t(2, itb)
			ivb3 = b%geom%t(3, itb)

			vb1  = b%geom%v(:, ivb1)
			vb2  = b%geom%v(:, ivb2)
			vb3  = b%geom%v(:, ivb3)

			call tri_line( &
				vb1, vb2, vb3, &
				va1, va2, &
				p, stat)

			if (stat == 0) then
				print *, "collision detected"
				print *, "p = ", p

				nr = nr + 1

				r = r + p

				! Normal vector is outward from body `b` (inward to body `a`)
				nrm_ = normalize(cross(vb2 - vb1, vb3 - vb1))
				nrm  = nrm + nrm_

				print *, "nrm_ = ", nrm_

			end if

		end do

	end do
	end do

	if (nr == 0) return  ! no collision

	r = r / nr

	!nrm = nrm / nr
	call normalize_s(nrm)

	print *, "r = ", r
	print *, "nrm = ", nrm
	!print *, "nr = ", nr

	! Get collision point coordinate relative to each bodies' center of mass
	r1 = r - a%pos
	r2 = r - b%pos

	print *, "r1 = ", r1
	print *, "r2 = ", r2

	! Collide body `a` (body 1) with `b` (body 2).  This is similar to the
	! body-to-ground collision case, but now there are two actual bodies

	! Velocities of each bodies' points *at point of contact* (not
	! center of mass)
	vp1 = a%vel + cross(a%ang_vel, r1)
	vp2 = b%vel + cross(b%ang_vel, r2)

	! Relative velocity
	vr = vp2 - vp1

	! Mass and inertia *in world frame of reference*
	m1 = a%mass
	m2 = b%mass
	i1 = matmul(matmul(a%rot, a%inertia), transpose(a%rot))
	i2 = matmul(matmul(b%rot, b%inertia), transpose(b%rot))

	! Sum of external forces acting on body.  TODO: dry up this calculation when
	! more forces are added (e.g. inter-body gravity).  It's already calculated
	! in update_body()
	fe1 = m1 * w%grav_accel
	fe2 = m2 * w%grav_accel
	!print *, "fe = ", fe

	! Tangent vector
	if (abs(dot_product(normalize(vr), nrm)) > tol) then
		tng = vr - dot_product(vr, nrm) * nrm
		call normalize_s(tng)

	! TODO: how should the next two cases be distinguished?  Maybe the condition
	! should be based on relative acceleration instead of relative forces?
	! Subtract accel from each other?
	else if (norm2(fe1) >= norm2(fe2) .and. &
		abs(dot_product(normalize(fe1), nrm)) > tol) then

		tng = fe1 - dot_product(fe1, nrm) * nrm
		call normalize_s(tng)

	else if (abs(dot_product(normalize(fe2), nrm)) > tol) then
		tng = fe2 - dot_product(fe2, nrm) * nrm
		call normalize_s(tng)

	else
		tng = 0
	end if
	!tng = -tng
	!print *, "tng = ", tng

	!********
	! Pack data into a matrix for lapack
	rhs(:,1) = cross(r1, nrm)
	rhs(:,2) = cross(r1, tng)

	tmp = invmul(i1, rhs)

	! Unpack
	i1_r1_nrm = tmp(:,1)
	i1_r1_tng = tmp(:,2)

	!********
	! Pack data into a matrix for lapack
	rhs(:,1) = cross(r2, nrm)
	rhs(:,2) = cross(r2, tng)

	tmp = invmul(i2, rhs)

	! Unpack
	i2_r2_nrm = tmp(:,1)
	i2_r2_tng = tmp(:,2)

	! Coefficient of restitution
	e = 0.5d0 * ( &
		w%matls(a%matl)%coef_rest + &
		w%matls(b%matl)%coef_rest &
	)

	! Normal impulse magnitude.  Ref: https://en.wikipedia.org/wiki/Collision_response
	jr_mag = -(1.d0 + e) * dot_product(vr, nrm) / &
		(1.d0 / m1 + 1.d0 / m2 + &
		dot_product(nrm, cross(i1_r1_nrm, r1) + &
		                 cross(i2_r2_nrm, r2)))
	!print *, "jr_mag = ", jr_mag

	! TODO: if the `e` fudge factor is changed, make sure to change the
	! body-to-ground case too
	jf_mag = -e * dot_product(vr, tng) / &
	!jf_mag = -(1.d0 + e) * dot_product(vr, tng) / &
	!jf_mag = -dot_product(vr, tng) / &
		(1.d0 / m1 +  + 1.d0 / m2 + &
		dot_product(tng, cross(i1_r1_tng, r1) + &
		                 cross(i2_r2_tng, r2)))

	friction_dyn = 0.5d0 * ( &
		w%matls(a%matl)%friction_dyn + &
		w%matls(b%matl)%friction_dyn &
	)

	! Apply friction cone clamp.  TODO: when should this be static friction?
	jf_max = friction_dyn * abs(jr_mag)
	jf_mag = clamp(jf_mag, -jf_max, jf_max)
	!print *, "jf_mag = ", jf_mag

	!********

	! TODO: avg vel damping like in ground collisions?
	a%vel = a%vel - jr_mag * nrm / m1 - jf_mag * tng / m1
	!a%vel = 0.5d0 * (a%vel0 + a%vel) - jr_mag * nrm / m1 - jf_mag * tng / m1

	a%ang_vel = a%ang_vel - jr_mag * i1_r1_nrm - jf_mag * i1_r1_tng

	!! Averaging positions seems to reduce chances of glitching into a stuck
	!! state
	!a%pos = a%pos0
	!a%rot = a%rot0
	a%pos = 0.5d0 * (a%pos + a%pos0)
	a%rot = 0.5d0 * (a%rot + a%rot0)
	call update_vertices(a)

	!********

	b%vel = b%vel + jr_mag * nrm / m2 + jf_mag * tng / m2
	!b%vel = 0.5d0 * (b%vel0 + b%vel) + jr_mag * nrm / m2 + jf_mag * tng / m2

	b%ang_vel = b%ang_vel + jr_mag * i2_r2_nrm + jf_mag * i2_r2_tng

	!b%pos = b%pos0
	!b%rot = b%rot0
	b%pos = 0.5d0 * (b%pos + b%pos0)
	b%rot = 0.5d0 * (b%rot + b%rot0)
	call update_vertices(b)

end subroutine collide_bodies

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

function invmul(a, b, iostat) result(x)

	! Solve the matrix equation:
	!
	!     A * x = b
	!
	! for x:
	!
	!     x = inv(A) * b

	double precision, intent(in) :: a(:,:), b(:,:)
	double precision, allocatable :: x(:,:)

	integer, intent(out), optional :: iostat

	!********

	double precision, allocatable :: a_(:,:)

	integer :: io, n, nrhs, lda, ldb
	integer, allocatable :: ipiv(:)

	!print *, "a = ", a
	!print *, "b = ", b

	! In general, these sizes could have different values if b is a matrix or A
	! is not square

	n = size(a, 2)

	nrhs = size(b, 2)
	lda = n
	ldb = n

	allocate(ipiv(n))

	x = b
	a_ = a
	call dgesv(n, nrhs, a_, lda, ipiv, x, ldb, io)

	if (present(iostat)) then
		iostat = io
	else
		if (io /= 0) call panic("lapack error in dgesv()")
	end if

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
	double precision :: norm
	norm = norm2(v)
	! TODO: DRY with subroutine version normalize_s
	if (norm > 1.d-12) then
		u = v / norm
	else
		allocate(u( size(v) ))
		u = 0.d0
	endif
end function normalize

!===============================================================================

subroutine normalize_s(v)
	double precision, intent(inout) :: v(:)
	double precision :: norm
	norm = norm2(v)
	if (norm > 1.d-12) then
		v = v / norm
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

	! Convert a vec3 ang to a rotation matrix rot

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

subroutine update_vertices(b)

	type(body_t), intent(inout)  :: b

	!********

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

end subroutine update_vertices

!===============================================================================

subroutine write_case(w)
	! Ref: https://dav.lbl.gov/archive/NERSC/Software/ensight/doc/Manuals/UserManual.pdf

	type(world_t), intent(in) :: w

	!********

	character(len = :), allocatable :: case_file, dir

	integer :: fid, io, i

	! TODO: add arg for dir and file basename.  Encapsulate in world struct?

	dir = "scratch"
	case_file =   dir//"/ribbit-1.case"
	io = make_dir(dir//NULL_CHAR)  ! ignore return code
	io = del_file(case_file//NULL_CHAR)

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

subroutine write_c80(fid, str)

	integer, intent(in) :: fid
	character(len = *), intent(in) :: str

	!********

	character :: buf80*80

	buf80 = str
	buf80(len(buf80): len(buf80)) = LINE_FEED
	write(fid) buf80

end subroutine write_c80

!===============================================================================

subroutine write_step(w)
	! Ref: https://dav.lbl.gov/archive/NERSC/Software/ensight/doc/Manuals/UserManual.pdf

	type(world_t), intent(in) :: w

	!********

	character(len = :), allocatable :: geom_file, dir

	integer :: fid, io, i, ib

	real, allocatable :: r(:,:)

	! TODO: add json (or cmd) binary toggle option
	logical, parameter :: binary = .true.

	!print *, "starting write_step()"

	! TODO: add arg for dir and file basename.  Encapsulate in world struct?

	dir = "scratch"
	geom_file =   dir//"/ribbit-1-"//to_str(w%it)//".geo"
	io = make_dir(dir//NULL_CHAR)  ! ignore return code
	io = del_file(geom_file//NULL_CHAR)

	if (binary) then

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

			write(fid) [(r(1,i), i = 1, size(r,2))]
			write(fid) [(r(2,i), i = 1, size(r,2))]
			write(fid) [(r(3,i), i = 1, size(r,2))]

			call write_c80(fid, "tria3")
			write(fid) w%bodies(ib)%geom%nt
			write(fid) w%bodies(ib)%geom%t

		end do

		close(fid)

	else

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

			write(fid, "(es16.6)") [(r(1,i), i = 1, size(r,2))]
			write(fid, "(es16.6)") [(r(2,i), i = 1, size(r,2))]
			write(fid, "(es16.6)") [(r(3,i), i = 1, size(r,2))]

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

