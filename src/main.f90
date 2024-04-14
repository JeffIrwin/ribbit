
module ribbit_app

	use ribbit

	type args_t

		character(len = :), allocatable :: ribbit_file

		logical :: &
			ribbit_file_arg = .false., &
			permissive      = .false., &
			version         = .false., &
			help            = .false.

	end type args_t

	!********

contains

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

function read_args() result(args)

	! This argument parser is based on http://docopt.org/
	!
	! c.f. github.com/jeffirwin/cali and syntran

	type(args_t) :: args

	!********

	character(len = :), allocatable :: argv, str, url, version

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
		!	call get_next_arg(i, str)
		!	read(str, *, iostat = io) args%maxerr
		!	if (io /= exit_success) then
		!		write(*,*) ERROR_STR//"--fmax-errors "//str &
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
		write(*,*) "	ribbit <file.ribbit> [-p]"
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

end module ribbit_app

!===============================================================================

program main

	use ribbit
	use ribbit_app

	implicit none

	type(args_t)  :: args
	type(world_t) :: world

	!call unit_test_split()

	args  = read_args()
	if (args%help .or. args%version) then
		call ribbit_exit(EXIT_SUCCESS)
	end if

	world = read_world(args%ribbit_file, args%permissive)
	call init_world(world)

	call ribbit_run(world)
	call ribbit_exit(EXIT_SUCCESS)

end program main

!===============================================================================

