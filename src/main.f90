
module ribbit

	use iso_fortran_env
	implicit none

	!********

	integer, parameter :: ND = 3

	integer, parameter :: &
		ERR_JSON_SYNTAX = -1, &
		ERR_LOAD_JSON   = -2

	! TODO: color
	character(len = *), parameter :: ERROR_STR = "Error: "

	!********

	type body_t

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

contains

!===============================================================================

subroutine ribbit_main()

	use json_module

	double precision :: p0(ND), v0(ND)

	character(len = :), allocatable :: filename

	integer :: ib, i, io
	integer :: fid

	logical :: found

	type(json_file) :: json

	type(body_t)  :: b
	type(world_t) :: w

	write(*,*) "starting ribbit_main()"

	!w%grav_accel = [0.d0, 0.d0, -9.807d0]
	!w%ground_z = 0.d0
	!b%pos = [0.d0, 0.d0, 1.d0]
	!b%vel = 0.d0
	!b%coef_rest = 0.95d0
	!allocate(w%bodies(1))
	!w%bodies(1) = b
	!w%t_start = 0.d0
	!w%t_end   = 1.d0
	!w%dt      = 0.0005d0

	! initialize the class
	call json%initialize()

	! read the file

	filename = "./inputs/bouncy-ball.ribbit"
	filename = "./inputs/bouncy-balls.ribbit"

	call json%load(filename = filename)
	if (json%failed()) then
		write(*,*) "Error:"
		write(*,*) "Could not load file """//filename//""""
		write(*,*)
		call json%print_error_message()
		write(*,*)
		io = ERR_LOAD_JSON
		! TODO: make function and use io as return val
		return
	end if

	! One number per line in arrays.  Not great for template matrix
	call json%print()

	call json%traverse(traverse_world)
	if (io /= 0) return

	!! TODO
	!return

	open(newunit = fid, file = "dump2.csv")

	w%t = w%t_start
	do while (w%t <= w%t_end)

		print *, "t, z = ", w%t, w%bodies(1)%pos(3)
		write(fid, *) w%t, w%bodies(1)%pos(3), w%bodies(2)%pos(3)

		do ib = 1, size(w%bodies)
			b = w%bodies(ib)

			v0 = b%vel
			b%vel = v0 + w%grav_accel * w%dt

			p0 = b%pos
			b%pos = p0 + 0.5 * (v0 + b%vel)
			if (b%pos(3) < w%ground_z) then

				b%pos = p0

				!b%vel(3) = -b%vel(3)
				b%vel(3) =  -b%coef_rest * v0(3)

			end if

			w%bodies(ib) = b
		end do

		w%t = w%t + w%dt
	end do

	write(*,*) "ending ribbit_main()"

contains

!===============================================================================

subroutine traverse_world(json, p, finished)

	! Based on the traverser here:
	!
	!     https://github.com/jacobwilliams/json-fortran/issues/204
	!

	!use json_module

	class(json_core), intent(inout)       :: json
	type(json_value), pointer, intent(in) :: p
	logical(json_LK), intent(out)         :: finished

	!********

	character(kind=json_CK, len=:), allocatable :: key, sval, path

	integer(json_IK) :: ival, count_, count_gc, i, igc, index_
	integer, allocatable :: template(:), t2(:,:)

	logical(json_LK) :: found

	type(json_core) :: core
	type(json_value), pointer :: pc, pp, pgc

	! Get the name of the key and the type of its value
	call json%info(p, name = key)
	!call json%info(p, name = key, path = path)
	!call json%get_path(p,path,found)  ! JSON-style
	call json%get_path(p, path)

	!print *, "key = """//key//""""
	!print *, "path = """//path//""""

	!!case_: select case (key)
	!case_: select case (path)
	case_: if (path == "world") then
		! Ignore non-leaf nodes
	!else if (path == "world.bodies") then

	else if (path == "world.dt") then
		call json%get(p, "@", w%dt)
		print *, "w%dt = ", w%dt

	else if (path == "world.t_start") then
		call json%get(p, "@", w%t_start)
		print *, "w%t_start = ", w%t_start

	else if (path == "world.t_end") then
		call json%get(p, "@", w%t_end)
		print *, "w%t_end = ", w%t_end

	else if (path == "world.grav_accel") then

		w%grav_accel = get_array(json, p, ND)
		print *, "grav_accel = ", w%grav_accel

	else if (path == "world.bodies") then
		count_ = json%count(p)
		print *, "bodies count = ", count_
		allocate(w%bodies(count_))

		do ib = 1, count_
			call json%get_child(p, ib, pc)

			print *, "traversing body ", ib
			print *, "{"

			! "grandchildren" count
			count_gc = json%count(pc)
			print *, "count_gc = ", count_gc

			do igc = 1, count_gc
				call json%get_child(pc, igc, pgc)
				call json%info(pgc, name = key)

				select case (key)

				case ("pos")
					w%bodies(ib)%pos = get_array(json, pgc, ND)
					print *, "w%bodies(ib)%pos", w%bodies(ib)%pos

				case ("vel")
					w%bodies(ib)%vel = get_array(json, pgc, ND)
					print *, "w%bodies(ib)%vel", w%bodies(ib)%vel

				case ("coef_rest")
					call json%get(pgc, "@", w%bodies(ib)%coef_rest)

				case default
					! TODO
					print *, "bad key ", key

				end select

			end do

			print *, "}"
			print *, "done"
			print *, ""

		end do

	else

		if (key == filename) exit case_
		if (key == ""      ) exit case_
		if (starts_with(path, "world.bodies")) exit case_

		! TODO: consider making this an error, unless running with a "loose
		! syntax" cmd arg.  Same idea for unknown cmd args.  Allowing unknown
		! keys is good for future compatibility but bad for users who might make
		! typos.

		write(*,*) "Warning:  unknown JSON path"
		write(*,*) "Path    :"""//path//""""
		write(*,*)

	!end select case_
	end if case_

	if (json%failed()) then

		write(*,*) "Error:"
		write(*,*) "Could not load file """//filename//""""
		write(*,*)
		call json%print_error_message()
		write(*,*)
		finished = .true.
		io = ERR_JSON_SYNTAX

	end if

	! always false, since we want to traverse all nodes:
	finished = .false.

end subroutine traverse_world

!===============================================================================

end subroutine ribbit_main

!===============================================================================

function get_array(json, p, n) result(array)
	use json_module

	type(json_core) :: json
	type(json_value), pointer, intent(in) :: p
	integer, intent(in) :: n

	double precision :: array(n)

	!********

	integer :: i, io, count_
	type(json_value), pointer :: pc

	count_ = json%count(p)
	if (count_ /= n) then
		write(*,*) "Error:"
		! TODO: get path and quote it for error message
		write(*,*) "array must have 3 components"
		!finished = .true.
		io = ERR_JSON_SYNTAX
		call exit(io)
	end if

	do i = 1, count_
		call json%get_child(p, i, pc)
		call json%get(pc, "@", array(i))
	end do

end function get_array

!===============================================================================

logical function starts_with(str_, prefix)
	character(len = *), intent(in) :: str_, prefix
	starts_with = str_(1: min(len(str_), len(prefix))) == prefix
end function starts_with

!===============================================================================

end module ribbit

!===============================================================================

program main
	use ribbit
	implicit none

	call ribbit_main()

end program main

