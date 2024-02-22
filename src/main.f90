
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

function read_json(filename) result(w)
	use json_module

	character(len = *), intent(in) :: filename
	type(world_t) :: w

	!********

	character(kind=json_CK, len=:), allocatable :: key, sval, path

	integer :: io, ib
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
		write(*,*) "Error:"
		write(*,*) "Could not load file """//filename//""""
		write(*,*)
		call json%print_error_message()
		write(*,*)
		io = ERR_LOAD_JSON
		! TODO: make function and use io as return val
		call exit(io)
	end if

	call json%print()


	call core%initialize()
	call json%get(pw)
	!call json%get(pw, "world")
	!call json%get_by_path(pw, "world")
	call core%get_child(pw, pw)
	!! TODO: traverse root to check typos, name of "world"

	count_w = core%count(pw)
	print *, "count world children = ", count_w

	do ic = 1, count_w
	call core%get_child(pw, ic, p)
	call core%info(p, name = key)

	case_: select case (key)

	case ("dt")
		call core%get(p, "@", w%dt)
		print *, "w%dt = ", w%dt

	case ("t_start")
		call core%get(p, "@", w%t_start)
		print *, "w%t_start = ", w%t_start

	case ("t_end")
		call core%get(p, "@", w%t_end)
		print *, "w%t_end = ", w%t_end

	case ("grav_accel")

		w%grav_accel = get_array(core, p, ND)
		print *, "grav_accel = ", w%grav_accel

	case ("bodies")
		count_ = core%count(p)
		print *, "bodies count = ", count_
		allocate(w%bodies(count_))

		do ib = 1, count_
			call core%get_child(p, ib, pc)

			print *, "traversing body ", ib
			print *, "{"

			! "grandchildren" count
			count_gc = core%count(pc)
			print *, "count_gc = ", count_gc

			do igc = 1, count_gc
				call core%get_child(pc, igc, pgc)
				call core%info(pgc, name = key)

				select case (key)

				case ("pos")
					w%bodies(ib)%pos = get_array(core, pgc, ND)
					print *, "w%bodies(ib)%pos", w%bodies(ib)%pos

				case ("vel")
					w%bodies(ib)%vel = get_array(core, pgc, ND)
					print *, "w%bodies(ib)%vel", w%bodies(ib)%vel

				case ("coef_rest")
					call core%get(pgc, "@", w%bodies(ib)%coef_rest)

				case default
					! TODO
					print *, "bad key ", key

				end select

			end do

			print *, "}"
			print *, "done"
			print *, ""

		end do

	case default

		if (key == filename) exit case_
		if (key == ""      ) exit case_
		!if (starts_with(path, "world.bodies")) exit case_

		! TODO: consider making this an error, unless running with a "loose
		! syntax" cmd arg.  Same idea for unknown cmd args.  Allowing unknown
		! keys is good for future compatibility but bad for users who might make
		! typos.

		write(*,*) "Warning:  unknown JSON key"
		write(*,*) "Key    :"""//key//""""
		write(*,*)

	end select case_
	end do

	if (core%failed()) then

		write(*,*) "Error:"
		write(*,*) "Could not load file """//filename//""""
		write(*,*)
		call core%print_error_message()
		write(*,*)
		io = ERR_JSON_SYNTAX

	end if

	if (io /= 0) call exit(io)

end function read_json

!===============================================================================

subroutine ribbit_main()

	use json_module

	double precision :: p0(ND), v0(ND)

	character(len = :), allocatable :: filename

	integer :: ib, i, io
	integer :: fid

	logical :: found

	!type(json_file) :: json

	type(body_t)  :: b
	type(world_t) :: w

	write(*,*) "starting ribbit_main()"

	! read the file

	filename = "./inputs/bouncy-ball.ribbit"
	filename = "./inputs/bouncy-balls.ribbit"

	w = read_json(filename)

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

	write(*,*) "ending ribbit_main()"

contains

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

