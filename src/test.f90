
module test_m

	use ribbit
	implicit none

contains

!===============================================================================

subroutine unit_test_tri_line()

	double precision :: diff_norm
	double precision :: a(ND), b(ND), c(ND), e(ND), f(ND)
	double precision :: p(ND), pexpect(ND), diff(ND)
	double precision :: fs(ND, 6), ps(ND, 6)

	integer :: i, stat, stat_expect
	integer, allocatable :: stats(:)

	write(*,*) "starting unit_test_tri_line()"

	! Points a, b, and c form the triangle
	a = [1.d0, 0.d0, 0.d0]
	b = [0.d0, 2.d0, 0.d0]
	c = [0.d0, 0.d0, 3.d0]

	! Points e and f form the line segment
	e = [0.2d0, 0.1d0, 0.3d0]

	fs(:,1) = [ 0.5d0,  1.5d0,  1.0d0]  ! valid intersection
	fs(:,2) = [ 0.4d0,  1.3d0,  1.2d0]  ! valid intersection
	fs(:,3) = [-0.5d0, -1.5d0, -1.0d0]  ! outside of line segment
	fs(:,4) = [ 1.0d0, -1.0d0,  3.0d0]  ! outside of triangle
	fs(:,5) = [-0.8d0,  2.1d0,  0.3d0]  ! exactly parallel

	! Expected unit test results
	stats = [0, 0, -1, -1, -2]
	ps(:,1) = [0.358108108108108d0, 0.837837837837838d0, 0.668918918918919d0]
	ps(:,2) = [0.318181818181818d0, 0.809090909090909d0, 0.831818181818182d0]

	do i = 1, 5
		f = fs(:,i)
		pexpect = ps(:,i)
		stat_expect = stats(i)

		write(*,*) ""
		!write(*,*) "f = ", f
		call tri_line(a, b, c, e, f, p, stat)
		write(*,*) "stat = ", stat

		if (stat /= stat_expect) then
			call panic("tri_line test `"//to_str(i)//"` failed. got bad stat")
		end if

		if (stat /= 0) cycle
		write(*,*) "p = ", p

		diff = p - pexpect
		!write(*,*) "diff = ", diff

		diff_norm = norm2(diff)
		write(*,*) "diff_norm = ", diff_norm

		if (diff_norm > 1.d-13) then
			call panic("tri_line test `"//to_str(i)//"` failed. got bad p coordinate")
		end if

	end do

	write(*,*)
	write(*,*) "ending unit_test_tri_line()"

end subroutine unit_test_tri_line

!===============================================================================

subroutine test_run()

	write(*,*) fg_bright_magenta//"starting test_run()"//color_reset

	! TODO: make integration tests

	call unit_test_tri_line()

	write(*,*) fg_bright_magenta//"ending test_run()"//color_reset

end subroutine test_run

!===============================================================================

end module test_m

!===============================================================================

program main

	use ribbit
	use test_m

	implicit none

	!type(world_t) :: world

	call test_run()
	call ribbit_exit(EXIT_SUCCESS)

end program main

!===============================================================================

