MODULE o_lagrange
	implicit none
	integer(4), parameter :: Deep = 1, jl = 11, il = 11
	integer(4), parameter :: N_PARTICLES = 1 ! number of particles
	real(4), parameter :: radius = 6378100.0, pi = 3.141592653589793238
	integer(4) :: AllocateStatus, i, j
	integer(4) :: i_west, j_south, i_east, j_north, rank

	real(8),allocatable :: x1_v (:,:), x2_v(:,:) 
	real(8),allocatable :: u_c (:,:,:), v_c(:,:,:) 
	! Particles coordinates
	real(8),allocatable ::  x1_p (:)! longitude of advected particles
	real(8),allocatable ::  x2_p (:)! latitude  of advected particles

	! Trajectories
	real(4),allocatable ::  x1_trek(:,:)   ! trajectories of all particles will be stored in these arrays
	real(4),allocatable ::  x2_trek(:,:)   ! for all timesteps. 1st index = timestep, 2nd = # of particle
	real(4) :: dt

contains
	SUBROUTINE o_alloc_check(stat)
		integer(4), intent(in) :: stat
		IF (stat /= 0) THEN
			PRINT *, 'Not enough memory'			
			STOP
		END IF
	END SUBROUTINE

	SUBROUTINE o_alloc_data
		print *, i_west, i_east, j_south,j_north
		ALLOCATE ( x1_v(i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)		
		ALLOCATE ( x2_v(i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)		
		ALLOCATE ( u_c(Deep,i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)		
		ALLOCATE ( v_c(Deep,i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)
		ALLOCATE ( x1_p(N_PARTICLES), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)
		ALLOCATE ( x2_p(N_PARTICLES), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)
	END SUBROUTINE

	SUBROUTINE o_ini_data
		call o_fill_grid
		call o_fill_u_c
	END SUBROUTINE

	SUBROUTINE o_get_size(wi, he)
		integer, intent(out) :: wi, he
		wi = il
		he = jl
	END SUBROUTINE

	SUBROUTINE o_fill_indexes(south_j, north_j, west_i, east_i, rank_proc)
		integer, intent(in) :: south_j, north_j, west_i, east_i, rank_proc
		i_west = west_i
		j_south = south_j
	   	i_east = east_i
		j_north = north_j
		rank = rank_proc
	END SUBROUTINE


	SUBROUTINE o_fill_grid
		do j = j_south, j_north, 1
			do i = i_west, i_east, 1
				x1_v(i, j) = 0. + real(i)
				x2_v(i, j) = 0. + real(j)
			end do
		end do
		x1_p(1) = 7
		x2_p(1) = 4
		dt = 1
	END SUBROUTINE

	SUBROUTINE o_fill_u_c
		real(4) :: wc, hc, rmax, x, y, r, u, v, cos_alfa, sin_alfa
	! vortex center
		wc = (il-1)/2.0
		hc = (jl-1)/2.0

	! vortex size
		rmax = sqrt( wc**2 + hc**2)
	! set currents	
		do j = j_south, j_north, 1
			do i = i_west, i_east, 1			
				x = x1_v(i,j)
				y = x2_v(i,j)
				if (x == wc .and. y == hc) then
					u_c(1,i,j) = 0.
					v_c(1,i,j) = 0.
				else
					r = sqrt( (x-wc)**2 + (y-hc)**2 )  ! polar coordinates
					cos_alfa = (wc-x) / r	   ! of the grid node					
					sin_alfa = (y-hc) / r
					u_c(1,i,j) =  r/rmax * sin_alfa
					v_c(1,i,j) =  r/rmax * cos_alfa
				end if
			end do
		end do
	! set zeros at boundaries
	!u_c(:,:,1) = 0. ; u_c(:,:,jl) = 0.
	!v_c(:,:,1) = 0. ; v_c(:,:,jl) = 0.
	!u_c(:,1,:) = 0. ; u_c(:,il,:) = 0.
	!v_c(:,1,:) = 0. ; v_c(:,il,:) = 0.
	END SUBROUTINE

	SUBROUTINE o_print_data
		print *, 'GRID'
		do j = j_south, j_north, 1
			do i = i_west, i_east, 1
				write (*,'(f5.2, A, f5.2,A$)'), x1_v(i,j),'|', x2_v(i,j),'|||'
			end do
			print*,''
		end do
		print *, 'SPEED'
		do j = j_south, j_north, 1
			do i = i_west, i_east, 1
				write (*,'(f5.2, A, f5.2,A$)'), u_c(1,i,j),'|', v_c(1,i,j),'|||'
			end do
			print*,''
		end do
	END SUBROUTINE

	SUBROUTINE o_print_file
		character(len=1024) :: filename
		write (filename, "(A,I2,A)") "files_ver0/greed_speed", rank, ".txt"
		print *, 'PRINT TO FILE GRID AND SPEED'
		do j = j_south, j_north, 1
			do i = i_west, i_east, 1
				open(13,FILE=filename)
				write (13,'(f10.6,A,f10.6,A,f10.6,A,f10.6)'), x1_v(i,j),';', x2_v(i,j),';', u_c(1, i, j),';',& 
				v_c(1,i,j)
			end do
		end do
	END SUBROUTINE

	SUBROUTINE o_dealloc_data
		 DEALLOCATE ( x1_v, STAT = AllocateStatus)
		 DEALLOCATE ( x2_v, STAT = AllocateStatus)
		 DEALLOCATE ( u_c, STAT = AllocateStatus)
		 DEALLOCATE ( v_c, STAT = AllocateStatus)
	END SUBROUTINE

	! 3  4!	  !1 2!
	! 1  2!-->!3 4!
	SUBROUTINE o_bilinear_interp(x, y, x1,y1,x2,y2,f3,f4,f1,f2, res)
		integer(4), intent(in) :: x1, x2, y1, y2
		real(8), intent(in) :: x, y
		real(4), intent(in) :: f1, f2, f3, f4
		real(4), intent(out) :: res
		res = f1 * (x2-x) * (y2 - y)/((x2-x1)*(y2-y1)) +  f2 * (x-x1) * (y2 - y)/((x2-x1)*(y2-y1)) +&
			 f3 * (x2-x) * (y - y1)/((x2-x1)*(y2-y1)) +  f4 * (x-x1) * (y - y1)/((x2-x1)*(y2-y1))
	END SUBROUTINE

	SUBROUTINE get_nearest_node(x, res)
		real(8), intent(in) :: x
		integer(4), intent(out) :: res
		res = FLOOR(x)
	END SUBROUTINE

	SUBROUTINE o_update
		real(4) u_c_inter, v_c_inter
		real(4) u(4), v(4)
		integer(4) n
		integer(4) x1(N_PARTICLES), x2(N_PARTICLES), x3(N_PARTICLES), x4(N_PARTICLES)
		integer(4) y1(N_PARTICLES), y2(N_PARTICLES), y3(N_PARTICLES), y4(N_PARTICLES)
		integer(4) ip(N_PARTICLES), jp(N_PARTICLES)
		do n=1,N_PARTICLES
			call get_nearest_node(x1_p(n), x1(n))
			call get_nearest_node(x2_p(n), y1(n))
		end do
		x2 = x1 + 1
		y2 = y1
		x3 = x1
		y3 = y1 + 1
		x4 = x1 + 1
		y4 = y1 + 1
		do n=1, N_PARTICLES
			u(1) = u_c(DEEP,x1(n)  ,y1(n))
			u(2) = u_c(DEEP,x2(n)  ,y2(n))
			u(3) = u_c(DEEP,x3(n)  ,y3(n))
			u(4) = u_c(DEEP,x4(n)  ,y4(n))

			v(1) = v_c(DEEP,x1(n)  ,y1(n))
			v(2) = v_c(DEEP,x2(n)  ,y2(n))
			v(3) = v_c(DEEP,x3(n)  ,y3(n))
			v(4) = v_c(DEEP,x4(n)  ,y4(n))

			call o_bilinear_interp(x1_p(n),x2_p(n),x3(n),y3(n),x2(n),y2(n),u(1),u(2),u(3),u(4),u_c_inter) 
	    		call o_bilinear_interp(x1_p(n),x2_p(n),x3(n),y3(n),x2(n),y2(n),v(1),v(2),v(3),v(4),v_c_inter)	
	
			x1_p(n) = x1_p(n) + u_c_inter*dt!(u_c_inter*dt*180.)/(radius*pi*cos(x2_p(n)/180.*pi))
			x2_p(n) = x2_p(n) + v_c_inter*dt!(v_c_inter*dt*180.)/(radius*pi)
			
			open(12,FILE='files_ver0/track.txt')			
			write(12,'(f10.5,A,f10.5,A,f10.5,A,f10.5)') ,x1_p(1),';', x2_p(1),';',u_c_inter,';',v_c_inter

		end do
	END SUBROUTINE
END MODULE

MODULE divide_grid
	implicit none
	integer(4) :: proc_count = 0
	contains
	SUBROUTINE g_fill_indexes(rank, w_min, w_max, h_min, h_max, j_south, j_north, i_west, i_east)
		integer, intent(in) :: w_min, w_max, h_min, h_max, rank
		integer, intent(out) :: j_south, j_north, i_west, i_east
		integer(4) :: step_w, step_h
		integer(4) :: i, j

		step_w = (w_max-w_min)/proc_count
		step_h = (h_max-h_min)/proc_count
		if (rank == 0) then
			j_south = 1 + mod(rank, proc_count) * step_h 
			j_north = 1 + (mod(rank, proc_count) + 1) * step_h
			i_west = 1 + rank / proc_count * step_w
			i_east = 1 + (rank / proc_count + 1) * step_w
		else if (mod(rank, proc_count) == 0) then
			j_south = 1 + mod(rank, proc_count) * step_h 
			j_north = 1 + (mod(rank, proc_count) + 1) * step_h
			i_west = 2 + rank / proc_count * step_w
			i_east = 1 + (rank / proc_count + 1) * step_w
		else if (rank / proc_count == 0)  then
			j_south = 2 + mod(rank, proc_count) * step_h 
			j_north = 1 + (mod(rank, proc_count) + 1) * step_h
			i_west = 1 + rank / proc_count * step_w
			i_east = 1 + (rank / proc_count + 1) * step_w
		else
			j_south = 2 + mod(rank, proc_count) * step_h 
			j_north = 1 + (mod(rank, proc_count) + 1) * step_h
			i_west = 2 + rank / proc_count * step_w
			i_east = 1 + (rank / proc_count + 1) * step_w
		endif		
	END SUBROUTINE

	SUBROUTINE g_axis_count_proc(proc_count_all)
		integer(4), intent(in) :: proc_count_all 
		integer(4) :: i		
		do i = 1,10,1
			if (i*i == proc_count_all) THEN
				proc_count = i
			endif
		enddo
		if (proc_count == 0) THEN
			print *, 'enter integer square digit as count of proc. Default: grid with 1 kernel'
			proc_count = 1
		endif
		if (proc_count_all > 100) THEN
			print *, 'enter digit <= 100 as count of proc. Default: grid with 1 kernel'
			proc_count = 1
		endif
	END SUBROUTINE
END MODULE

PROGRAM main
	use o_lagrange
	use divide_grid
	include 'mpif.h' 
	integer(4) :: ierr, rank_proc, count_processes
	integer(4) :: Width, Height
	integer(4) :: south, north, west, east
	call o_get_size(Width, Height)
	open (10, FILE = 'files_ver0/grid&rank_info.txt')
	write (10, '(I2,A,I2,A,I2)'), 1,';', Width,';', Height
	call o_fill_indexes(1,Height,1,Width,0)
	call o_alloc_data
	call o_ini_data	
	call o_print_data
	call o_print_file
	do i=1,15
		call o_update
	end do
	call o_dealloc_data
END PROGRAM
