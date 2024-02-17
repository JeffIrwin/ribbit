
module ribbit

	implicit none

	!********

	integer, parameter :: ND = 3

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

subroutine ribbit_main

	type(world_t) :: w
	type(body_t)  :: b

	double precision :: p0(ND), v0(ND)

	integer :: ib
	integer :: fid

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

end subroutine ribbit_main

!===============================================================================
end module ribbit
!===============================================================================

program main
	use ribbit
	implicit none

	call ribbit_main()

end program main

