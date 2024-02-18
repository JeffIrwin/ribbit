
module ribbit

	use iso_fortran_env
	implicit none

	!********

	integer, parameter :: ND = 3

	integer, parameter :: &
		ERR_JSON_SYNTAX = -1, &
		ERR_LOAD_JSON   = -2

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

		double precision :: floor_z

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
	!type(json_core) :: core
	!type(json_value), pointer :: p !! a pointer for low-level manipulations

	type(body_t)  :: b
	type(world_t) :: w

	write(*,*) "starting ribbit_main()"

	w%grav_accel = [0.d0, 0.d0, -9.807d0]
	w%floor_z = 0.d0

	b%pos = [0.d0, 0.d0, 1.d0]
	b%vel = 0.d0

	b%coef_rest = 0.95d0

	allocate(w%bodies(1))
	w%bodies(1) = b

	w%t_start = 0.d0
	w%t_end   = 1.d0
	w%dt      = 0.0005d0

	! initialize the class
	call json%initialize()

	! read the file
	!call json%load(filename = '../files/inputs/test1.json')
	filename = "./inputs/bouncy-ball.ribbit"
	call json%load(filename = filename)
	if (json%failed()) then
		write(*,*) 'Error:'
		write(*,*) 'Could not load file "'//filename//'"'
		write(*,*)
		call json%print_error_message()
		write(*,*)
		io = ERR_LOAD_JSON
		! TODO: make function and use io as return val
		return
	end if

	! One number per line in arrays.  Not great for template matrix
	call json%print()

	call json%traverse(traverse_ribbit_json)
	if (io /= 0) return

	!! print the file to the console
	!call json%print()

	!call core%initialize()
	!call json%get(p) ! get root

	!call core%initialize(unescape_strings=.false., compact_reals=.true., &
	!	real_format='*')
	!!call core%traverse(p, traverser)
	!!call core%world_traverse(p, w)

	!call core%world_traverse(p)
	!call world_traverse(core, p, w)

	!! extract data from the file
	!! [found can be used to check if the data was really there]
	!call json%get('version.major', i, found)
	!if ( .not. found ) call exit(-1)

	! TODO:
	return

	open(newunit = fid, file = "dump.csv")

	w%t = w%t_start
	do while (w%t <= w%t_end)

		print *, "t, z = ", w%t, w%bodies(1)%pos(3)
		write(fid, *) w%t, w%bodies(1)%pos(3)

		do ib = 1, size(w%bodies)
			b = w%bodies(ib)

			v0 = b%vel
			b%vel = v0 + w%grav_accel * w%dt

			p0 = b%pos
			b%pos = p0 + 0.5 * (v0 + b%vel)
			if (b%pos(3) < w%floor_z) then

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

subroutine traverse_ribbit_json(json, p, finished)

	! Based on the traverser here:
	!
	!     https://github.com/jacobwilliams/json-fortran/issues/204
	!

	!use json_module

	class(json_core), intent(inout)       :: json
	type(json_value), pointer, intent(in) :: p
	logical(json_LK), intent(out)         :: finished

	!********

	character(kind=json_CK, len=:), allocatable :: key, sval

	integer(json_IK) :: ival, ncount, ij
	integer, allocatable :: template(:), t2(:,:)

	logical(json_LK) :: found

	!real(json_RK) :: rvalx, rvaly
	!type(json_value), pointer :: pc

	! Get the name of the key and the type of its value
	call json%info(p, name = key)

	!print *, 'key = "'//key//'"'

	if (key == "world") then

		print *, 'found world'
		!! Colormap file
		!call json%get(p, '@', s%fcolormap)

	!else if (key == colormap_id) then
	!	! Colormap name
	!	call json%get(p, '@', s%colormap)

	!else if (key /= "" .and. key /= s%fjson) then
	else if (key /= "") then

		! TODO: consider making this an error, unless running with a "loose
		! syntax" cmd arg.  Same idea for unknown cmd args.  Allowing unknown
		! keys is good for future compatibility but bad for users who might make
		! typos.

		write(*,*) 'Warning:  unknown JSON key'
		write(*,*) 'Key    :"'//key//'"'
		write(*,*)

	end if

	if (json%failed()) then

		write(*,*) 'Error:'
		write(*,*) 'Could not load file "'//filename//'"'
		write(*,*)
		call json%print_error_message()
		write(*,*)
		finished = .true.
		io = ERR_JSON_SYNTAX

	end if

	! always false, since we want to traverse all nodes:
	finished = .false.

end subroutine traverse_ribbit_json

!===============================================================================

end subroutine ribbit_main

!===============================================================================

	subroutine world_traverse(json, p, w)

	use json_module
	implicit none

	!class(json_core),intent(inout)         :: json
	type(json_core),intent(inout)         :: json

	type(json_value),pointer,intent(in)    :: p
	!procedure(json_traverse_callback_func) :: traverse_callback

	type(world_t), intent(inout) :: w

	logical :: finished !! can be used to stop the process

	!if (.not. json%exception_thrown) call traverser(p)
	if (.not. json%failed()) call traverser(p, w)

	contains

		recursive subroutine traverser(p, w)

		!! recursive [[json_value]] traversal.

		implicit none

		type(json_value),pointer,intent(in) :: p
		type(world_t), intent(inout) :: w

		character(len=:),allocatable :: path !! path to the variable
		character(len=:),allocatable :: value !! variable value as a string

		integer :: i, var_type
		integer :: icount   !! number of children

		logical :: found

		type(json_value),pointer :: child !! variable's first child

		type(json_value),pointer :: element  !! a child element

		!if (json%exception_thrown) return
		if (json%failed()) return

		call traverse_cbk(json, p, finished, w) ! first call for this object

		if (finished) return

		!for arrays and objects, have to also call for all children:
		call json%info(p,var_type=var_type)
		if (var_type==json_array .or. var_type==json_object) then

			icount = json%count(p) ! number of children
			if (icount>0) then

				!element => p%children   ! first one
				!call json%get_child(p,child)
				call json%get_child(p,element)

				do i = 1, icount        ! call for each child
					if (.not. associated(element)) then
						call json%throw_exception('Error in world_traverse: '//&
							'Malformed JSON linked list')
						return
					end if
					call traverser(element, w)
					!if (finished .or. json%exception_thrown) exit
					if (finished .or. json%failed()) exit

					!element => element%next
					!call json%get_next(p_integer_array,p_tmp)  !should be "names"
					!call json%get_next(p, element)
					call json%get_next(element, element)

				end do
			end if
			nullify(element)

		end if

		end subroutine traverser

	end subroutine world_traverse

!===============================================================================

subroutine traverse_cbk(json, p, finished, w)

	!! A `traverse` routine for printing out all
	!! the variables in a JSON structure.

	use json_module
	implicit none

	class(json_core),intent(inout)      :: json
	type(json_value),pointer,intent(in) :: p
	logical, intent(out) :: finished  !! set true to stop traversing
	type(world_t), intent(inout) :: w

	character(len=:),allocatable :: path !! path to the variable
	logical :: found !! error flag
	type(json_value),pointer :: child !! variable's first child
	character(len=:),allocatable :: value !! variable value as a string
	integer :: var_type !! JSON variable type

	call json%get_child(p,child)
	finished = .false.

	! only print the leafs:
	if (.not. associated(child)) then
		call json%get_path(p,path,found,&
				use_alt_array_tokens=.true.,&
				path_sep='%')  ! fortran-style

		if (found) then

			call json%info(p,var_type=var_type)
			select case (var_type)
			case (json_array)
				!an empty array
				value = '()'
			case (json_object)
				!an empty object
				value = '{}'
			case default
				! get the value as a string
				! [assumes strict_type_checking=false]
				! note: strings are returned escaped without quotes
				call json%get(p,value)
			end select

			!check for errors:
			if (json%failed()) then
				finished = .true.
			else

				! TODO: save to world arg w
				write(output_unit,'(A)') path//' = '//value

			end if

		else
			finished = .true.
		end if
	end if

end subroutine traverse_cbk

!===============================================================================
end module ribbit
!===============================================================================

program main
	use ribbit
	implicit none

	call ribbit_main()

end program main

