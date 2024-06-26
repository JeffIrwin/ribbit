
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

		double precision :: scale

		double precision :: pos(ND)
		double precision :: vel(ND)

		double precision :: rot(ND, ND)
		double precision :: ang_vel(ND)

		double precision :: vol, mass
		double precision :: inertia(ND, ND)

		! previous state
		double precision :: pos0(ND), vel0(ND), rot0(ND, ND)

		! Bounding box.  Could be in geom instead of body
		double precision :: box(ND,2)

		!double precision :: force(ND)  ! net force *on* the body
		double precision :: acc(ND)

	end type body_t

	!********

	type world_t

		! The world contains physical constants, initial conditions (within
		! bodies), and also numerical settings

		double precision :: grav_accel(ND)  ! uniform grav accel
		double precision :: grav_const      ! Newtonian constant of gravitation

		type(body_t), allocatable :: bodies(:)
		type(matl_t), allocatable :: matls (:)

		double precision :: ground_pos(ND)
		double precision :: ground_nrm(ND)
		logical :: has_ground

		! Input time bounds and step size
		double precision :: t_start, t_end, dt

		double precision  :: t   ! transient time value for current step
		integer(kind = 8) :: it  ! time index

		! Symplectic integrator constants.  TODO: add a json option to select
		! the integrator order
		double precision, allocatable :: vel_coefs(:), acc_coefs(:)

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

	write(*,*)

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

	! Apply scaling, then get mass, center of mass, volume, and inertia tensor.
	! Maybe this should be renamed init_body()

	type(body_t), intent(inout) :: b
	type(world_t), intent(in)   :: w

	!********

	double precision :: vol_tet, com_tet(ND), vol, com(ND), dens
	double precision :: v(ND, 4), x(4), y(4), z(4)
	double precision :: ixx, iyy, izz, ixy, iyz, izx

	integer :: i, j, k

	b%geom%v = b%geom%v * b%scale

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

	if (b%mass > 0) then
		! Mass can be set as an input instead of density
		dens = b%mass / b%vol
	else
		dens = w%matls(b%matl)%dens
	end if

	b%inertia = b%inertia * dens
	write(*,*) "b%inertia = "
	write(*, "(3es16.6)") b%inertia

	! Translate all vertices so that the center of mass is at the origin
	do i = 1, b%geom%nv
		b%geom%v(:,i) = b%geom%v(:,i) - com
	end do

	! Backup original vertex locations so that rounding errors during rotation
	! do not warp the shape of the body
	b%geom%v0 = b%geom%v

	b%mass = dens * b%vol

	!b%acc = 0.d0

	write(*,*) "vol  = ", b%vol
	write(*,*) "com  = ", com
	write(*,*) "mass = ", b%mass

	! TODO: log bounding box summary

	write(*,*)

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
	w%grav_const = 0.d0
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

	case ("grav_const")
		call json%get(p, "@", w%grav_const)

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
			w%bodies(ib)%scale   = 1.d0
			w%bodies(ib)%pos     = 0.d0
			w%bodies(ib)%vel     = 0.d0
			w%bodies(ib)%ang_vel = 0.d0
			w%bodies(ib)%matl = 1
			has_geom = .false.
			ang = 0.d0

			! If mass is 0, then it's calculated from density and volume
			w%bodies(ib)%mass = 0.d0

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

				case ("scale")
					call json%get(pgc, "@", w%bodies(ib)%scale)

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

				case ("mass")
					call json%get(pgc, "@", w%bodies(ib)%mass)

				case ("matl")
					call json%get(pgc, "@", w%bodies(ib)%matl)
					!print *, "matl = ", w%bodies(ib)%matl

				case default
					call bad_key(key, permissive)
				end select

				!! could apply scaling here, but i don't like how deeply it's
				!! nested in the structs
				!w%bodies(ib)%geom%v = w%bodies... * ...scale

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

	double precision :: c14, c24, d14, d24
	integer :: ib

	do ib = 1, size(w%bodies)
		call get_inertia(w%bodies(ib), w)
	end do

	!! Ruth 1983 (3rd order)
	!w%vel_coefs = [1.d0, -2.d0/3.d0, 2.d0/3.d0]
	!w%acc_coefs = [-1.d0/24.d0, 3.d0/4.d0, 7.d0/24.d0]

	c14 = 1.d0 / (2.d0 * (2.d0 - 2.d0 ** (1.d0/3.d0)))
	c24 = (1.d0 - 2.d0 ** (1.d0/3.d0)) / (2.d0 * (2.d0 - 2.d0 ** (1.d0/3.d0)))
	d14 = 1.d0 / (2.d0 - 2.d0 ** (1.d0/3.d0))
	d24 = -2.d0 ** (1.d0/3.d0) / (2.d0 - 2.d0 ** (1.d0/3.d0))

	w%vel_coefs = [c14, c24, c24, c14 ]
	w%acc_coefs = [d14, d24, d14, 0.d0]

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

	double precision :: t, bu, bv, bw, min_tri, min_lin, max_tri, max_lin
	double precision :: u(ND), v(ND), ef(ND), params(4)
	double precision :: mat(ND, ND), rhs(ND, 1)
	double precision, allocatable :: lhs(:,:)
	double precision :: box_tol, line_tol

	integer :: i, io

	! Check bounding boxes first.  For two spheres with ~1000 triangles, this is
	! about 10 times faster than unconditionally doing the linear algebra
	box_tol = 0.01
	do i = 1, ND

		min_tri = min(a(i), b(i), c(i))
		max_tri = max(a(i), b(i), c(i))

		min_lin = min(e(i), f(i))
		max_lin = max(e(i), f(i))

		! TODO: make box_tol a percentage instead of absolute
		if (max_lin < min_tri - box_tol .or. &
			max_tri < min_lin - box_tol) then
			stat = -3
			return
		end if

	end do

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

subroutine ribbit_run(w, dump_csv_)

	use json_module
	type(world_t), intent(inout) :: w
	logical, intent(in) :: dump_csv_

	!********

	character(len = : ), allocatable :: csv_file

	integer :: io
	integer :: fid

	write(*,*) "starting ribbit_run()"

	if (dump_csv_) then
		! TODO: set filename based on input.  Refactor out of ribbit_run()
		csv_file = "dump.csv"
		open(newunit = fid, file = csv_file, action = "write", iostat = io)
		call handle_open_write_io(csv_file, io)
	end if

	w%t = w%t_start
	w%it = 0
	call init_bodies(w)
	call write_step(w)

	! TODO: run an initial check to see if bodies are already colliding. Panic?

	do while (w%t <= w%t_end)

		!print *, "t, z = ", w%t, w%bodies(1)%pos(3)
		print *, "w%it = ", w%it

		call dump_csv             (w, dump_csv_, fid)
		call cache_bodies         (w)

		!call sum_acc_bodies       (w)
		call integrate_bodies     (w)
		call update_bodies        (w)

		call collide_ground_bodies(w)
		call collide_bodies       (w)

		w%it = w%it + 1
		w%t  = w%t  + w%dt
		call write_step(w)

	end do
	call write_case(w)

	write(*,*) "number of time steps = ", w%it
	write(*,*) "ending ribbit_run()"

end subroutine ribbit_run

!===============================================================================

subroutine dump_csv(w, dump_csv_, fid)
	type(world_t), intent(inout) :: w
	logical, intent(in) :: dump_csv_
	integer, intent(in) :: fid

	integer :: ib

	if (.not. dump_csv_) return

	write(fid, "(es16.6)", advance = "no") w%t
	do ib = 1, size(w%bodies)
		write(fid, "(3es16.6)", advance = "no") w%bodies(ib)%pos
		write(fid, "(3es16.6)", advance = "no") w%bodies(ib)%vel
		! could also add rot/ang_vel if there's a need
	end do
	write(fid, *)

end subroutine dump_csv

!===============================================================================

subroutine init_bodies(w)
	type(world_t), intent(inout) :: w
	integer :: ib
	do ib = 1, size(w%bodies)
		call update_vertices(w%bodies(ib))
	end do
end subroutine init_bodies

!===============================================================================

subroutine cache_bodies(w)
	type(world_t), intent(inout) :: w
	integer :: ib
	do ib = 1, size(w%bodies)
		call cache_body(w%bodies(ib))
	end do
end subroutine cache_bodies

!===============================================================================

subroutine update_bodies(w)
	type(world_t), intent(inout) :: w
	integer :: ib
	do ib = 1, size(w%bodies)
		call update_body(w, w%bodies(ib))
	end do
end subroutine update_bodies

!===============================================================================

subroutine collide_ground_bodies(w)
	type(world_t), intent(inout) :: w
	integer :: ib
	do ib = 1, size(w%bodies)
		call collide_ground_body(w, w%bodies(ib))
	end do
end subroutine collide_ground_bodies

!===============================================================================

subroutine collide_bodies(w)
	type(world_t), intent(inout) :: w
	integer :: ia, ib
	do ib = 1, size(w%bodies)
		do ia = 1, size(w%bodies)
		!do ia = 1, ib - 1
		!do ia = ib + 1, size(w%bodies)
			if (ia == ib) cycle
			call collide_body_pair(w, w%bodies(ia), w%bodies(ib))
			!call collide_body_pair(w, w%bodies(ib), w%bodies(ia))
		end do
	end do
end subroutine collide_bodies

!===============================================================================

subroutine cache_body(b)
	! Save initial state
	type(body_t), intent(inout) :: b

	b%pos0 = b%pos
	b%rot0 = b%rot

	b%vel0 = b%vel

end subroutine cache_body

!===============================================================================

subroutine sum_acc_bodies(w)
	type(world_t), intent(inout) :: w
	!********
	integer :: ia, ib

	do ib = 1, size(w%bodies)
		call init_acc(w, w%bodies(ib))

		! Iterate over unique pairs of bodies.  Note that this is different from
		! the loop in collide_bodies()
		do ia = 1, ib - 1
			call add_acc(w, w%bodies(ia), w%bodies(ib))
		end do

	end do

end subroutine sum_acc_bodies

!===============================================================================
subroutine integrate_bodies(w)

	! Integrate the equations of motion for Newtonian gravity using a symplectic
	! integrator
	!
	!     https://en.wikipedia.org/wiki/Symplectic_integrator#Examples
	!
	! Runge-Kutta is harder to implement because you need an array of "k"
	! coefficients (1 of each k for each body), and symplectic integrators are
	! better for equations of motion anyway

	type(world_t), intent(inout) :: w
	!********
	integer :: i

	do i = 1, size(w%vel_coefs)
		call increment_pos_bodies(w, w%vel_coefs(i))
		call sum_acc_bodies(w)
		call increment_vel_bodies(w, w%acc_coefs(i))
	end do

end subroutine integrate_bodies

!===============================================================================

subroutine increment_vel_bodies(w, acc_coef)
	type(world_t), intent(inout) :: w
	double precision, intent(in) :: acc_coef

	integer :: ib
	do ib = 1, size(w%bodies)
		call increment_vel_body(w, w%bodies(ib), acc_coef)
	end do

end subroutine increment_vel_bodies

!********

subroutine increment_vel_body(w, b, acc_coef)
	type(world_t), intent(in) :: w
	type(body_t), intent(inout) :: b
	double precision, intent(in) :: acc_coef

	b%vel = b%vel + acc_coef * b%acc * w%dt

end subroutine increment_vel_body

!===============================================================================

subroutine increment_pos_bodies(w, vel_coef)
	type(world_t), intent(inout) :: w
	double precision, intent(in) :: vel_coef

	integer :: ib
	do ib = 1, size(w%bodies)
		call increment_pos_body(w, w%bodies(ib), vel_coef)
	end do

end subroutine increment_pos_bodies

!********

subroutine increment_pos_body(w, b, vel_coef)
	type(world_t), intent(in) :: w
	type(body_t), intent(inout) :: b
	double precision, intent(in) :: vel_coef

	b%pos = b%pos + vel_coef * b%vel * w%dt

end subroutine increment_pos_body

!===============================================================================

subroutine init_acc(w, b)

	type(world_t), intent(in) :: w
	type(body_t), intent(inout) :: b

	b%acc = 0.d0
	b%acc = b%acc + w%grav_accel

end subroutine init_acc

!===============================================================================

subroutine add_acc(w, a, b)

	type(world_t), intent(in) :: w
	type(body_t), intent(inout) :: a, b

	!********

	double precision :: apos(ND), bpos(ND)
	double precision :: f(ND), r(ND), r2, force_dens(ND)

	! For perf, it might be better to move this to a `cycle` in
	! sum_acc_bodies().  But if we generalize e.g. to electro forces too, it
	! might be cleaner to have the checks here
	if (w%grav_const == 0) return

	! For symplectic integration, we want to use a "1st order approximation"
	! here.  The fancy stuff comes in at a higher level in increment_pos* and
	! increment_vel* fns
	apos = a%pos
	bpos = b%pos

	r = bpos - apos

	force_dens = w%grav_const / dot_product(r, r) * normalize(r)

	!print *, "f = ", f

	a%acc = a%acc + force_dens * b%mass
	b%acc = b%acc - force_dens * a%mass

end subroutine add_acc

!===============================================================================

subroutine update_body(w, b)

	! Update the pose of body `b` due to its linear and angular velocity, and
	! update its velocity due to net force, e.g. gravitational acceleration

	type(world_t), intent(in) :: w
	type(body_t), intent(inout) :: b

	!********

	double precision :: accel(ND)

	! Position and velocity updates have been moved to integrate_bodies()

	! Update rotations by multiplying by a rotation matrix, not by
	! adding vec3's!  Matmul is fine here for 3x3's.
	b%rot = matmul(get_rot(b%ang_vel * w%dt), b%rot)

	! Rounding errors might accumulate after many time steps.
	! Re-orthonormalize just in case
	call gram_schmidt(b%rot)

	call update_vertices(b)

end subroutine update_body

!===============================================================================

subroutine collide_ground_body(w, b)

	! Update body `b` due to collision with the global ground, which is like a
	! special symbolic body with infinite inertia and extents

	type(world_t), intent(in) :: w
	type(body_t), intent(inout) :: b

	!********

	double precision, parameter :: tol = 0.001  ! coincident vector angle-ish tol

	double precision :: i1_r1_nrm(ND), vr(ND), vp1(ND), vp2(ND), r1(ND), &
		nrm(ND), m1, i1(ND, ND), impulse_nrm, e, tng(ND), fe(ND), &
		impulse_tng, i1_r1_tng(ND), impulse_max

	double precision :: rhs(ND,2)
	double precision, allocatable :: tmp(:,:)

	integer :: i, ncolliding

	if (.not. w%has_ground) return

	! In case of collision along an entire face or edge, get the centroid of
	! that face/edge instead of just one vertex, by taking the average of all
	! colliding vertices
	ncolliding = 0
	r1 = 0.d0
	do i = 1, b%geom%nv
		if (dot_product(w%ground_nrm, b%geom%v(:,i) - w%ground_pos) <= 0) then
			ncolliding = ncolliding + 1
			r1 = r1 + b%geom%v(:,i) - b%pos
		end if
	end do
	if (ncolliding == 0) return
	r1 = r1 / ncolliding

	!print *, "ncolliding = ", ncolliding

	! Collide body (body 1) with ground (body 2).  The ground has
	! infinite mass and inertia, so many terms become zero

	! Velocities of each bodies' points *at point of contact* (not
	! center of mass)
	vp2 = 0
	vp1 = b%vel + cross(b%ang_vel, r1)

	! Relative velocity
	vr = vp2 - vp1

	! Normal vector of collision plane
	nrm = w%ground_nrm

	if (dot_product(vr, nrm) < 0) return

	! Mass and inertia *in world frame of reference*
	m1 = b%mass
	i1 = matmul(matmul(b%rot, b%inertia), transpose(b%rot))

	! Net force acting on body
	fe = b%acc * b%mass

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
	impulse_nrm = -(1.d0 + e) * dot_product(vr, nrm) / &
		(1.d0/m1 + dot_product(nrm, cross(i1_r1_nrm, r1)))
	!print *, "impulse_nrm = ", impulse_nrm

	! Ref:  https://gafferongames.com/post/collision_response_and_coulomb_friction/

	!impulse_tng = -e * dot_product(vr, tng) / &
	!impulse_tng = -(1.d0 + e) * dot_product(vr, tng) / &
	impulse_tng = -dot_product(vr, tng) / &
		(1.d0/m1 + dot_product(tng, cross(i1_r1_tng, r1)))

	! Apply friction cone clamp.  TODO: when should this be static friction?
	impulse_max = w%matls(b%matl)%friction_dyn * abs(impulse_nrm)
	impulse_tng = clamp(impulse_tng, -impulse_max, impulse_max)
	!print *, "impulse_tng = ", impulse_tng

	! Lerp velocity to add a little damping?
	#define LERP 1.d0
	b%vel = LERP * b%vel + (1.d0 - LERP) * b%vel0 - impulse_nrm * nrm / m1 - impulse_tng * tng / m1

	b%ang_vel = b%ang_vel - impulse_nrm * i1_r1_nrm - impulse_tng * i1_r1_tng

	!print *, "b%ang_vel", b%ang_vel

	! Pose lerp helps bodies damp and settle instead of making small bounces
	! forever
	#define LERP 0.5d0
	b%pos = LERP * b%pos + (1.d0 - LERP) * b%pos0
	b%rot = LERP * b%rot + (1.d0 - LERP) * b%rot0
	call update_vertices(b)

	!print *, ""

end subroutine collide_ground_body

!===============================================================================

subroutine collide_body_pair(w, a, b)

	! Check if any edge of body `a` collides with any face of body `b`.  If so,
	! update their outgoing velocity states

	type(world_t), intent(in) :: w
	type(body_t), intent(inout) :: a, b

	!********

	double precision, parameter :: tol = 0.001d0  ! coincident vector angle-ish tol

	double precision :: va1(ND), va2(ND), vb1(ND), vb2(ND), vb3(ND), p(ND), &
		r(ND), r1(ND), r2(ND), nrm(ND), vp1(ND), vp2(ND), vr(ND), m1, m2, &
		i1(ND,ND), i2(ND,ND), fe1(ND), fe2(ND), tng(ND), i1_r1_nrm(ND), &
		i1_r1_tng(ND), i2_r2_nrm(ND), i2_r2_tng(ND), e, impulse_nrm, impulse_tng, &
		friction_dyn, impulse_max, nrm_(ND), box_tol
	double precision :: rhs(ND,2)
	double precision, allocatable :: tmp(:,:)

	integer :: stat, i, ita, ie, iva1, iva2, itb, ivb1, ivb2, ivb3, nr

	!print *, "starting collide_body_pair()"
	!print *, "a%pos = ", a%pos
	!print *, "b%pos = ", b%pos
	!print *, ""

	! Return early if bounding box check passes.  Box is cached in
	! update_vertices().  This is a simple but powerful optimization.  A better
	! but more complex approach would use something like binary space
	! partitioning.  The simple approach does not work when bodies are separated
	! by a diagonal plane.
	box_tol = 0.01
	do i = 1, ND
		! TODO: make box_tol a percentage instead of absolute
		if (a%box(i,2) < b%box(i,1) - box_tol .or. &
			b%box(i,2) < a%box(i,1) - box_tol) then
			return
		end if
	end do

	! Iterate through each edge of body `a` and get the average position `r` and
	! normal `nrm` of the collision point in the global coordinate system
	r   = 0.d0
	nrm = 0.d0
	nr  = 0
	!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(a, b) REDUCTION(+: r, nrm, nr)
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
				!print *, "collision detected"
				!print *, "p = ", p

				nr = nr + 1

				r = r + p

				! Normal vector is outward from body `b` (inward to body `a`)
				nrm_ = normalize(cross(vb2 - vb1, vb3 - vb1))
				nrm  = nrm + nrm_

				!print *, "nrm_ = ", nrm_

			end if

		end do

	end do
	end do
	!$OMP END PARALLEL DO

	if (nr == 0) return  ! no collision

	print *, "collision at w%it = ", to_str(w%it)

	r = r / nr

	!nrm = nrm / nr
	call normalize_s(nrm)

	!print *, "r = ", r
	!print *, "nrm = ", nrm
	!print *, "nr = ", nr

	! Get collision point coordinate relative to each bodies' center of mass
	r1 = r - a%pos
	r2 = r - b%pos

	!print *, "r1 = ", r1
	!print *, "r2 = ", r2

	! Collide body `a` (body 1) with `b` (body 2).  This is similar to the
	! body-to-ground collision case, but now there are two actual bodies

	! Velocities of each bodies' points *at point of contact* (not
	! center of mass)
	vp1 = a%vel + cross(a%ang_vel, r1)
	vp2 = b%vel + cross(b%ang_vel, r2)

	! Relative velocity
	vr = vp2 - vp1

	! If the bodies are already moving away from each other, stop!  Collision
	! response was probably just calculated already in the last time step
	!
	! This definitely reduces glitchiness, but it's not perfect.  See
	! space-cubes in the current commit
	if (dot_product(vr, nrm) < 0) return

	! Mass and inertia *in world frame of reference*
	m1 = a%mass
	m2 = b%mass
	i1 = matmul(matmul(a%rot, a%inertia), transpose(a%rot))
	i2 = matmul(matmul(b%rot, b%inertia), transpose(b%rot))

	! Net force acting on bodies
	fe1 = a%acc * a%mass
	fe2 = b%acc * b%mass
	!print *, "fe = ", fe

	! Tangent vector
	if (abs(dot_product(normalize(vr), nrm)) > tol) then
		tng = vr - dot_product(vr, nrm) * nrm
		call normalize_s(tng)

	! TODO: how should the next two cases be distinguished?  Maybe the condition
	! should be based on relative acceleration instead of relative force?
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
	impulse_nrm = -(1.d0 + e) * dot_product(vr, nrm) / &
		(1.d0 / m1 + 1.d0 / m2 + &
		dot_product(nrm, cross(i1_r1_nrm, r1) + &
		                 cross(i2_r2_nrm, r2)))
	!print *, "impulse_nrm = ", impulse_nrm

	! If the `e` fudge factor is changed, make sure to change the body-to-ground
	! case too
	!impulse_tng = -e * dot_product(vr, tng) / &
	!impulse_tng = -(1.d0 + e) * dot_product(vr, tng) / &
	impulse_tng = -dot_product(vr, tng) / &
		(1.d0 / m1 +  + 1.d0 / m2 + &
		dot_product(tng, cross(i1_r1_tng, r1) + &
		                 cross(i2_r2_tng, r2)))

	friction_dyn = 0.5d0 * ( &
		w%matls(a%matl)%friction_dyn + &
		w%matls(b%matl)%friction_dyn &
	)

	! Apply friction cone clamp.  TODO: when should this be static friction?
	impulse_max = friction_dyn * abs(impulse_nrm)
	impulse_tng = clamp(impulse_tng, -impulse_max, impulse_max)
	!print *, "impulse_tng = ", impulse_tng

	!********

	a%vel = a%vel - impulse_nrm * nrm / m1 - impulse_tng * tng / m1

	a%ang_vel = a%ang_vel - impulse_nrm * i1_r1_nrm - impulse_tng * i1_r1_tng

	! TODO: remove LERP if 1 is ok
	#define LERP 1.0d0

	a%pos = LERP * a%pos + (1.d0 - LERP) * a%pos0
	a%rot = LERP * a%rot + (1.d0 - LERP) * a%rot0  ! TODO: gram_schmidt()?
	call update_vertices(a)

	!********

	b%vel = b%vel + impulse_nrm * nrm / m2 + impulse_tng * tng / m2

	b%ang_vel = b%ang_vel + impulse_nrm * i2_r2_nrm + impulse_tng * i2_r2_tng

	b%pos = LERP * b%pos + (1.d0 - LERP) * b%pos0
	b%rot = LERP * b%rot + (1.d0 - LERP) * b%rot0
	call update_vertices(b)

end subroutine collide_body_pair

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

	double precision, allocatable :: dummy(:,:)
	integer :: i

	! Rotate first and then translate by com position

	!print *, "rotating"

	!! Lapack dgemm() is better than matmul.  This crashes on large data (e.g.
	!! homer.obj) when compiled without "-heap-arrays0" ifx option, and it
	!! crashes unconditionally on even larger data even with the heap arrays
	!! option
	!b%geom%v = matmul(b%rot, b%geom%v0)

	!allocate(dummy( ND, b%geom%nv ))
	!dummy = 0.d0
	!b%geom%v = 0.d0

	!b%geom%v(1,:) = b%pos(1)
	!b%geom%v(2,:) = b%pos(2)
	!b%geom%v(3,:) = b%pos(3)

	do i = 1, b%geom%nv
		b%geom%v(1,i) = b%pos(1)
		b%geom%v(2,i) = b%pos(2)
		b%geom%v(3,i) = b%pos(3)
	end do

	! Rotate and translate in one operation.  TODO: add wrapper like invmul()?
	call dgemm("N", "N", ND, b%geom%nv, ND, 1.d0, b%rot, ND, b%geom%v0, ND, &
		1.d0, b%geom%v, ND)
	!b%geom%v = b%geom%v0
	!dgemm (transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)

	!print *, "translating"
	!do i = 1, ND
	!	b%geom%v(i,:) = b%geom%v(i,:) + b%pos(i)
	!end do

	do i = 1, ND
		b%box(i,1) = minval(b%geom%v(i,:))
		b%box(i,2) = maxval(b%geom%v(i,:))
	end do

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
	write(fid, "(es16.6)") [(dble(i), i = 0, w%it)]

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

