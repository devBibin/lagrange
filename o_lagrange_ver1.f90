! Module for using MPI
MODULE shared_module
	implicit none
	include 'mpif.h'
END MODULE shared_module

! Primitive module for grid divide
MODULE divide_grid
	implicit none
	integer(4) :: d_proc_count, x_l, y_l
	integer(4) :: d_rank
	contains

	SUBROUTINE g_factor(n_in, a, b)
		integer, intent(in) :: n_in
		integer, intent(out) :: a, b
		integer :: res(100)
		integer :: cnt, i, d, n 
		d = 2
		cnt = 0
		n = n_in
		do while (n > 1)
			if (mod(n,d) == 0) THEN
				cnt = cnt + 1
				res(cnt) = d
				n = n / d
			else
				d = d + 1
			end if
		end do
		a = 1
		b = 1
		do i = 1, cnt
			if (a > b) THEN
				b = b * res(i)
			else
				a = a * res(i)
			end if
		end do
	END SUBROUTINE
	

	! Get frame processors indexes
	SUBROUTINE g_get_neigbours(westrank, eastrank, southrank, northrank, serank, swrank, nerank, nwrank)
		integer(4), intent(out):: westrank, eastrank, southrank, northrank
		integer(4), intent(out):: serank, swrank, nerank, nwrank

		westrank = d_rank - 1
		eastrank = d_rank + 1
		southrank = d_rank - x_l
		northrank = d_rank + x_l
		serank =  d_rank - x_l + 1
		swrank = d_rank - x_l - 1
		nerank = d_rank + x_l + 1
		nwrank = d_rank + x_l -1
		if (mod(d_rank, x_l) == 0) THEN
			westrank = -1
			swrank = -1
			nwrank = -1
		end if
		if (mod(d_rank, x_l) == x_l-1) THEN
			eastrank = -1
			serank = -1
			nerank = -1
		end if
		if (northrank >= d_proc_count) THEN
			northrank = -1
			nerank = -1
			nwrank = -1
		end if
		if (southrank < 0) THEN
			southrank = -1
			serank = -1
			swrank = -1
		end if
		!if (d_rank == 2) THEN
		!	print *, nwrank, northrank, nerank
		!	print *, westrank, d_rank, eastrank  
		!	print *, swrank, southrank, serank
		!end if 
	END SUBROUTINE

	! Get grid indexes of process with defined rank
	SUBROUTINE g_get_indexes(count_proc, rank_proc, w_min, w_max, h_min, h_max, j_south, j_north, i_west, i_east)
		integer, intent(in) :: w_min, w_max, h_min, h_max, rank_proc, count_proc
		integer, intent(out) :: j_south, j_north, i_west, i_east
		real(8) :: step_w, step_h
		integer :: rank_w, rank_h
		d_rank = rank_proc
		d_proc_count = count_proc

		call g_factor(d_proc_count, x_l, y_l)

		rank_w = mod(d_rank, x_l)
		rank_h = d_rank/x_l

		step_w = (w_max-w_min) / dble(x_l)
		step_h = (h_max-h_min) / dble(y_l)

		i_west = CEILING(rank_w * step_w) + w_min
		i_east = CEILING((rank_w+1) * step_w) + w_min
		j_south = CEILING(rank_h * step_h) + h_min
		j_north = CEILING((rank_h+1) * step_h) + h_min
		!print *, d_rank, i_west, i_east, j_south, j_north
	END SUBROUTINE

	! Get grid indexes of process with defined rank
	SUBROUTINE g_get_indexes1(rank_proc, w_min, w_max, h_min, h_max, j_south, j_north, i_west, i_east)
		integer, intent(in) :: w_min, w_max, h_min, h_max, rank_proc
		integer, intent(in) :: j_south, j_north, i_west, i_east
		integer(4) :: step_w, step_h
		
	END SUBROUTINE
END MODULE

!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------


! Implementation of lagrange particle transport
MODULE o_lagrange
	use shared_module
	use divide_grid
	implicit none 	
	integer :: AllocateStatus, i, j, n					! Indexes and statuses	
	integer :: i_west, j_south, i_east, j_north, rank			! Subfield's indexes
	real(8),allocatable :: x1_v (:,:), x2_v(:,:) 				! Coordinates of the grid (1 - Width, 2 - height)
	real(8),allocatable :: u_c (:,:,:), v_c(:,:,:)				! Speeds in roots of the grid
	real(8),allocatable ::  x1_p (:), x2_p (:)				! Particles' current coordinate (1 - Width, 2 - height)
	real(8),allocatable :: x1_p_track (:,:), x2_p_track(:,:)		! Particles' trajectory
	real(8),allocatable :: p_nodes(:,:)					! cell particle info
	real(4),allocatable :: k_v(:,:)						! Landscape
	integer,allocatable :: flags(:)						! 0 - out, 1 - in, -1 - left map, -2 - on land, 2 - sending
	integer :: westrank, eastrank, southrank, northrank			! Frame neigbours indexes of processor. 
	integer :: serank, swrank, nerank, nwrank				! -1 - no neighbour
	integer :: count_proc							! Count of processors
	real (8), allocatable :: collector(:)					! Collector of tracks (only on main processor)
	! Constants (see set_default constants)
	real(4) :: DT								! Timestep
	integer :: MAIN_PROC, FREQUENCY_FOR_FILE				! Index of Main Processor| Write to file/iterations
	integer MY_COMM								! Communicator for MPI
	real(8) :: GRID_STEP, GRID_START_W, GRID_START_H			! Grid info (step, the most South-West point coordinates)
	integer :: DEEP, jl , il						! Grid parameters (size of map) 	
	integer :: N_P				 				! Number of particles
	integer :: TIMESTEPS							! Count of timesteps
	real(8), parameter :: RADIUS = 6378100.0, PI = 3.141592653589793238	! Standart constants
	integer :: LAND_MODE
<<<<<<< HEAD
	character(len=1024) :: FILENAME_PARTICLES
=======
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
contains
	!-----------------MEMORY SECTION----------------------!
	! Checking allocation
	SUBROUTINE o_alloc_check(stat)
		integer(4), intent(in) :: stat
		IF (stat /= 0) THEN
			PRINT *, 'Not enough memory'
		END IF
	END SUBROUTINE

	! Allocate arrays 
	SUBROUTINE o_alloc_data
		ALLOCATE ( x1_v(i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)		
		ALLOCATE ( x2_v(i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)		
		ALLOCATE ( u_c(DEEP,i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)		
		ALLOCATE ( v_c(DEEP,i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)
		ALLOCATE ( x1_p(N_P), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x1_p = -1
		ALLOCATE ( x2_p(N_P), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x2_p = -1
		ALLOCATE ( flags(N_P), STAT = AllocateStatus)
<<<<<<< HEAD
		call o_alloc_check(AllocateStatus); flags = 1
=======
		call o_alloc_check(AllocateStatus); flags = -3
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
		ALLOCATE ( x1_p_track(N_P, 0:FREQUENCY_FOR_FILE), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x1_p_track = -1
		ALLOCATE ( x2_p_track(N_P, 0:FREQUENCY_FOR_FILE), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x2_p_track = -1
		if (rank == MAIN_PROC) THEN
			ALLOCATE (collector(N_P*(2*FREQUENCY_FOR_FILE + 1)), STAT = AllocateStatus)
			call o_alloc_check(AllocateStatus); collector = -1
		end if
		ALLOCATE (p_nodes(N_P, 18), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); p_nodes = -1
		ALLOCATE (k_v(i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); k_v = -1
	END SUBROUTINE

	! Deallocate arrays
	SUBROUTINE o_dealloc_data
		 DEALLOCATE ( x1_v, STAT = AllocateStatus)
		 DEALLOCATE ( x1_v, STAT = AllocateStatus)
		 DEALLOCATE ( x1_p, STAT = AllocateStatus)
		 DEALLOCATE ( x2_p, STAT = AllocateStatus)
		 DEALLOCATE ( x1_p_track, STAT = AllocateStatus)
		 DEALLOCATE ( x2_p_track, STAT = AllocateStatus)
		 DEALLOCATE ( flags, STAT = AllocateStatus)
		 DEALLOCATE ( u_c, STAT = AllocateStatus)
		 DEALLOCATE ( v_c, STAT = AllocateStatus)
 		 DEALLOCATE ( p_nodes, STAT = AllocateStatus)
 		 if (rank == MAIN_PROC) THEN
			DEALLOCATE (collector, STAT = AllocateStatus)
		 end if
	END SUBROUTINE



	!--------------------------CONFIGURE SECTION--------------------------------!
	SUBROUTINE o_set_config(count_processes, rank_proc)
		integer, intent(in) :: count_processes, rank_proc
		integer(4) :: south, north, west, east
		integer(4) :: r_s, r_n, r_w, r_e, r_se, r_sw, r_ne, r_nw
		! Set constants
		call o_set_default_constants
	
		! Divide grid between processors, define neigbours' and border indexes 	
		call g_get_indexes(count_processes, rank_proc, 1, il, 1, jl, south, north, west, east)
		call o_set_indexes(south,north,west,east,rank_proc, count_processes)
		call g_get_neigbours(r_w, r_e, r_s, r_n, r_se, r_sw, r_ne, r_nw)
		call o_set_neigbours(r_w, r_e, r_s, r_n, r_se, r_sw, r_ne, r_nw)	
	END SUBROUTINE


	! Set essential model's constants
	SUBROUTINE o_set_default_constants
		integer :: is_from_file
<<<<<<< HEAD
		integer :: flag
		open(17,FILE='client/config')
		read(17,*), is_from_file
		if (is_from_file == 0) THEN
			read(17,*), il
			read(17,*), jl
			read(17,*), GRID_START_W
			read(17,*), GRID_START_H
			read(17,*), GRID_STEP
			read(17,*), TIMESTEPS
			read(17,*), DT
			read(17,*), MAIN_PROC
			read(17,*), FREQUENCY_FOR_FILE
			read(17,*), LAND_MODE
			read(17,'(A)'), FILENAME_PARTICLES
			read(17,*), N_P
			print *, N_P
		end if
		DEEP = 1			!deep of slice
		MY_COMM = MPI_COMM_WORLD	!communicator of module
		!MAIN_PROC = 0			!index of main process, which gather info
		!GRID_STEP = 0.01		!grid step (for stand alone)
		!GRID_START_W = 10		!start point for longitute
		!GRID_START_H = 33		!start point for latitude
		!TIMESTEPS = 10			!count of TIMESTEPS in model
		!DT = 100			!timestep
		!FREQUENCY_FOR_FILE = 10 	!write to file once per 100 iterations
		!LAND_MODE = 0			!0- if particle is drifting near the coast, 1 - if particle stays on land
		!print *, il, jl, GRID_START_W, GRID_START_H, GRID_STEP, TIMESTEPS, DT, MAIN_PROC, FREQUENCY_FOR_FILE, LAND_MODE
		!print *, ''
		close(17, STATUS='KEEP')
=======
		is_from_file = 999
		open(17,FILE='client/config')
		read(17,'(I)'), is_from_file
		if (is_from_file == 0) THEN
			read(17,'(I)'), il
			read(17,'(I)'), jl
			read(17,*), GRID_START_W
			read(17,*), GRID_START_H
			read(17,*), GRID_STEP
			read(17,'(I)'), TIMESTEPS
			read(17,*), DT
			read(17,'(I)'), MAIN_PROC
			!print *, GRID_START_W, DT
		end if
		N_P = 1 			!number of particles
		!MAIN_PROC = 0			!index of main process, which gather info
		MY_COMM = MPI_COMM_WORLD	!communicator of module
		!GRID_STEP = 0.01		!grid step (for stand alone)
		!GRID_START_W = 10		!start point for longitute
		!GRID_START_H = 33		!start point for latitude
		!TIMESTEPS = 1			!count of TIMESTEPS in model
		!DT = 100			!timestep
		DEEP = 1			!deep of slice
		FREQUENCY_FOR_FILE = 10 	!write to file once per 100 iterations
		LAND_MODE = 0			!0- if particle is drifting near the coast, 1 - if particle stays on land
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
		open(12,FILE='tracks/tracks.txt')
	END SUBROUTINE

	! Set grid indexes for each processor
	SUBROUTINE o_set_indexes(south_j, north_j, west_i, east_i, rank_proc, count_p)
		integer, intent(in) :: south_j, north_j, west_i, east_i, rank_proc, count_p
		i_west = west_i
		j_south = south_j
	   	i_east = east_i
		j_north = north_j
		rank = rank_proc
		count_proc = count_p
	END SUBROUTINE

	! Set indexes for frame processors
	SUBROUTINE o_set_neigbours(n_w, n_e, n_s, n_n, n_se, n_sw, n_ne, n_nw)
		integer(4), intent(in) :: n_w, n_e, n_s, n_n, n_se, n_sw, n_ne, n_nw
		westrank = n_w
		eastrank = n_e
		southrank = n_s
		northrank = n_n
		serank = n_se
		swrank = n_sw
		nerank = n_ne
		nwrank = n_nw
	END SUBROUTINE



	!------------------------PREPARE DATA SECTION-------------------------------!
	SUBROUTINE o_ini_data
		call o_fill_grid
		call o_fill_u_c
		call o_fill_particles
	END SUBROUTINE

	! Fill grid
	SUBROUTINE o_fill_grid
		do j = j_south, j_north, 1
			do i = i_west, i_east, 1
				x1_v(i, j) = GRID_START_W + GRID_STEP * real(i)
				x2_v(i, j) = GRID_START_H + GRID_STEP * real(j) 
			end do
		end do
	END SUBROUTINE

	! Calculate anticyclone and fill vectors' speed field
	SUBROUTINE o_fill_u_c
		real(4) :: wc, hc, rmax, x, y, r, u, v, cos_alfa, sin_alfa

		wc = GRID_START_W+(il*GRID_STEP)/2.0
		hc = GRID_START_H+(jl*GRID_STEP)/2.0

		rmax = sqrt( (wc-GRID_START_W)**2 + (hc-GRID_START_H)**2)

		do j = j_south, j_north, 1
			do i = i_west, i_east, 1			
				x = x1_v(i,j)
				y = x2_v(i,j)
				if (x == wc .and. y == hc) then
					u_c(1,i,j) = 0.
					v_c(1,i,j) = 0.
				else
					r = sqrt( (x-wc)**2 + (y-hc)**2 ) 
					cos_alfa = (wc-x) / r	  				
					sin_alfa = (y-hc) / r
					u_c(1,i,j) = r/rmax * sin_alfa
					v_c(1,i,j) = r/rmax * cos_alfa
				end if
			end do
		end do
	END SUBROUTINE

	! Fill particles coordinates
	SUBROUTINE o_fill_particles
		integer :: x1_root, x2_root
<<<<<<< HEAD
		integer :: flag
		real(8) :: x, y
		
		open(21, FILE = "client/particles.txt")
		flag = 0
		i = 0
		do while (flag == 0)
			if (i /= 0) THEN
				x1_p(i) = x
				x2_p(i) = y
				print *, x1_p(i), x2_p(i)
			end if
			read(21,*,IOSTAT = flag) x, y
			i = i + 1	
		end do

		do i=1,N_P
			call is_in_subdomain(x1_p(i), x2_p(i), i)
=======
		x1_p = 2.2 * GRID_STEP + GRID_START_W
		x2_p = 2.2 * GRID_STEP + GRID_START_H

		!x1_p(2) = 3 * GRID_STEP + GRID_START_W
		!x2_p(2) = 8.8 * GRID_STEP + GRID_START_H

		!x1_p(3) = 4.4 * GRID_STEP + GRID_START_W
		!x2_p(3) = 8.8 * GRID_STEP + GRID_START_H

		do i=1,N_P
			call is_in_subdomain_start(x1_p(i), x2_p(i), i)
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
			if (flags(i) == 1) THEN
				call dihotomic(x1_p(i), x2_p(i), x1_root, x2_root)
				p_nodes(i, 1) = x1_root
				p_nodes(i, 2) = x2_root
				call set_cell_data(0, 0, i)			
			end if
		end do
	END SUBROUTINE


	!-------------------------------PRINT SECTION-------------------------------------------!
	! Print inforamation about each node to file (Width,height)
	SUBROUTINE o_print_file
		character(len=1024) :: filename
		write (filename, "(A,I4,A)") "tracks/greed_speed", rank, ".txt"
		!PRINT TO FILE GRID AND SPEED
		do j = j_south, j_north, 1
			do i = i_west, i_east, 1
				open(13,FILE=filename)
				write (13,'(f10.6,A,f10.6,A,f10.6,A,f10.6,A,f10.6)'), x1_v(i,j),';', x2_v(i,j),';', u_c(1, i, j),';',& 
				v_c(1,i,j),';', k_v(i,j)
			end do
		end do

		! PRINT TO FILE SUBFIELDS INFO
		write (filename, "(A,I4,A)") "tracks/subfields", rank, ".txt"		
		open (11, file=filename)
		write (11,'(I2, A, I4, A, I4, A, I4, A, I4)'), rank,';', i_west,';', j_south,';', i_east,';', j_north
		
		! PRINT TO FILE CONFIGURE INFO
		if (rank == MAIN_PROC) THEN
			open (10, FILE = 'tracks/grid&rank_info.txt')
			write (10, '(I5,A,I5,A,I5,A,f15.10,A,f15.10,A,f15.10)'), count_proc,';', il,';', jl,';',GRID_STEP,';',GRID_START_W, ';',GRID_START_H
		end if
	END SUBROUTINE

	SUBROUTINE o_print_file_tracks(col_count)
		integer, intent(in) :: col_count
		integer :: ind 
		! PRINT TO FILE TRACKS
		if (rank == MAIN_PROC) THEN
			j = 1
			do while (j <= N_P * (2*col_count + 1))
				if (mod(j, 2*col_count+1) == 1) THEN
					ind = collector(j)
				else
					if (collector(j) /= -1) THEN
						write (12, '(I10,A,f15.6,A,f15.6)'),ind,';',collector(j),';', collector(j+col_count)
					end if
				end if
				if (mod(j,((ind-1)*(2*col_count+1) + col_count + 1)) == 0) THEN
					j = j + col_count + 1
				else
					j = j + 1 
				end if
			end do
		end if
	END SUBROUTINE

	!-----------------------------------MAIN SECTION------------------------------------------------------!
	!-----------------------------------AUXILARY FUNCTIONS------------------------------------------------!
	!-----------------------------------WORK WITH cellS---------------------------------------------------!
	! Find index of particle's cell at start step
	SUBROUTINE dihotomic(long, lat, x1_root, x2_root)
		real(8), intent(in) :: long, lat
		integer(4), intent(out) :: x1_root, x2_root
		integer :: med
		real (8) :: temp
		integer (4) :: left, right
		real (8) :: x
		
		! First iteration for longitute, second for latitude
		do j = 1, 2
			if (j == 1) THEN
				left = i_west
				right = i_east
				x = long
			else
				left = j_south
				right = j_north
				x = lat
			end if
			do while ((right - left) /= 1)
				med = CEILING((right + left) / 2.0)
				if (j == 1) THEN
					temp = x1_v(med,j_south)
				else
					temp = x2_v(i_west,med)
				end if
				if (temp > x) THEN
					right = med
				else
					left = med
				end if
			end do
			if (j == 1) THEN
				x1_root = left
			else
				x2_root = right
			end if
		end do
	END SUBROUTINE

	! Check if particle is in cell (which constrained with 4 nodes)
	SUBROUTINE is_in_cell(w, h, id, is_gone)
		integer(4), intent(in) :: id
		integer(4), intent(out) :: is_gone
		real(8),intent(in)  :: w, h

		if (w >= p_nodes(id,3) .and. w < p_nodes(id,5) .and. h >= p_nodes(id, 4) .and. h < p_nodes(id, 6)) THEN
			is_gone = 0
		else
			is_gone = 1
		end if
	END SUBROUTINE

	! Choosing cell where current particle left for
	SUBROUTINE choose_cell(w, h, id, we, sn)
		integer(4), intent(in) :: id
		real(8),intent(in)  :: w, h
		integer(4), intent(out) :: we, sn		
		real (8) :: border_e, border_w, border_s, border_n

		border_w = p_nodes(id, 3)
		border_s = p_nodes(id, 4)
		border_e = p_nodes(id, 5)
		border_n = p_nodes(id, 6)
		
		if (w < border_w) THEN
			if (h >= border_n) THEN
				we = -1
				sn = 1
			else if (h < border_s) THEN
				we = -1
				sn = -1
			else
				we = -1
				sn = 0	
			end if
		else if (w >= border_e) THEN
			if (h >= border_n) THEN
				we = 1
				sn = 1
			else if (h < border_s) THEN
				we = 1
				sn = -1
			else
				we = 1
				sn = 0
			end if
		else
			if (h >= border_n) THEN
				we = 0
				sn = 1
			else if (h < border_s) THEN
				we = 0
				sn = -1
			end if
		end if
	END SUBROUTINE

	! Updating particle's cell data
	SUBROUTINE set_cell_data(we, sn, id)
		integer(4), intent(in) :: we, sn, id
		integer(4) :: x1_root, x2_root

		x1_root = p_nodes(id,1) + we
		x2_root = p_nodes(id,2) + sn
		! index
		p_nodes(id, 1) = x1_root
		p_nodes(id, 2) = x2_root
		! left bottom				
		p_nodes(id, 3) = x1_v(x1_root, j_north)
		p_nodes(id, 4) = x2_v(i_west, x2_root-1)
		! right up				
		p_nodes(id, 5) = x1_v(x1_root + 1, j_north)
		p_nodes(id, 6) = x2_v(i_west, x2_root)
		! particle frame speeds
		! u
		p_nodes(id, 7) = u_c(DEEP, x1_root, x2_root)
		p_nodes(id, 8) = u_c(DEEP, x1_root+1, x2_root)
		p_nodes(id, 9) = u_c(DEEP, x1_root, x2_root-1)
		p_nodes(id, 10) = u_c(DEEP, x1_root+1, x2_root-1)
		! v
		p_nodes(id, 11) = v_c(DEEP, x1_root, x2_root)
		p_nodes(id, 12) = v_c(DEEP, x1_root+1, x2_root)
		p_nodes(id, 13) = v_c(DEEP, x1_root, x2_root-1)
		p_nodes(id, 14) = v_c(DEEP, x1_root+1, x2_root-1)
		! kv
		p_nodes(id, 15) = k_v(x1_root, x2_root)
		p_nodes(id, 16) = k_v(x1_root+1, x2_root)
		p_nodes(id, 17) = k_v(x1_root, x2_root-1)
		p_nodes(id, 18) = k_v(x1_root+1, x2_root-1)

		! if kv > 0 then land, so uv = 0
		if (k_v(x1_root, x2_root) > 0) THEN
			p_nodes(id, 7) = 0
			p_nodes(id, 11) = 0
		end if
		! uv (right up)
		if (k_v(x1_root+1, x2_root) > 0) THEN
			p_nodes(id, 8) = 0
			p_nodes(id, 12) = 0
		end if
		! uv (left bottom)
		if (k_v(x1_root, x2_root-1) > 0) THEN
			p_nodes(id, 9) = 0
			p_nodes(id, 13) = 0
		end if
		! uv (right bottom)
		if (k_v(x1_root+1, x2_root-1) > 0) THEN
			p_nodes(id, 10) = 0
			p_nodes(id, 14) = 0
		end if	
	END SUBROUTINE

	! update speeds in cell nodes
	SUBROUTINE update_speeds(id)
		integer, intent(in) :: id
		integer(4) :: x1_root, x2_root
		x1_root = p_nodes(id,1)
		x2_root = p_nodes(id,2)
		! u
		p_nodes(id, 7) = u_c(DEEP, x1_root, x2_root)
		p_nodes(id, 8) = u_c(DEEP, x1_root+1, x2_root)
		p_nodes(id, 9) = u_c(DEEP, x1_root, x2_root-1)
		p_nodes(id, 10) = u_c(DEEP, x1_root+1, x2_root-1)
		! v
		p_nodes(id, 11) = v_c(DEEP, x1_root, x2_root)
		p_nodes(id, 12) = v_c(DEEP, x1_root+1, x2_root)
		p_nodes(id, 13) = v_c(DEEP, x1_root, x2_root-1)
		p_nodes(id, 14) = v_c(DEEP, x1_root+1, x2_root-1)
	END SUBROUTINE

	!---------------------------------WORK WITH SUBDOMAINS-------------------------------------------------!
	! Check if particle in processor's area
	SUBROUTINE is_in_subdomain(w,h,id)
		integer, intent(in) :: id
		real(8),intent(in)  :: w, h
		if (w >= x1_v(i_west,j_south) .and. w < x1_v(i_east,j_south) .and. h >= x2_v(i_west,j_south) .and. h < x2_v(i_east,j_north)) THEN
			flags(id) = 1
		else
			flags(id) = 0
		end if
	END SUBROUTINE

<<<<<<< HEAD
=======
	SUBROUTINE is_in_subdomain_start(w,h,id)
		integer, intent(in) :: id
		real(8),intent(in)  :: w, h
		if (w >= x1_v(i_west,j_south) .and. w < x1_v(i_east,j_south) .and. h >= x2_v(i_west,j_south) .and. h < x2_v(i_east,j_north)) THEN
			flags(id) = 1
		end if
	END SUBROUTINE

>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
	! Defining area where particle left for
	SUBROUTINE choose_subdomain(w, h, subfield)
		integer(4), intent(out) :: subfield
		real(8),intent(in)  :: w, h		
		real (8) :: border_e, border_w, border_s, border_n

		border_w = x1_v(i_west, j_north)
		border_n = x2_v(i_west, j_north)
		border_e = x1_v(i_east, j_south)
		border_s = x2_v(i_east, j_south)

		if (w < border_w) THEN
			if (h >= border_n) THEN
				subfield = nwrank
			else if (h < border_s) THEN
				subfield = swrank
			else
				subfield = westrank			
			end if
		else if (w >= border_e) THEN
			if (h >= border_n) THEN
				subfield = nerank
			else if (h < border_s) THEN
				subfield = serank
			else
				subfield = eastrank		
			end if
		else
			if (h >= border_n) THEN
				subfield = northrank
			else if (h < border_s) THEN
				subfield = southrank
			end if
		end if
	END SUBROUTINE


	! bilinear interpolation
	! x, y - coordinates of the particle
	! x1, y1 - left bottom node
	! x2, y2 - right upper node
	! f1, f2, f3, f4 - left-up, right-up, left-down, right-down
	SUBROUTINE o_bilinear_interp(x, y, x1,y1,x2,y2,f3,f4,f1,f2, res)
		real(8), intent(in) :: x1, x2, y1, y2
		real(8), intent(in) :: f1, f2, f3, f4
		real(8), intent(in) :: x, y
		real(8), intent(out) :: res
<<<<<<< HEAD

=======
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
		res = f1 * (x2-x) * (y2 - y)/((x2-x1)*(y2-y1)) +  f2 * (x-x1) * (y2 - y)/((x2-x1)*(y2-y1)) +&
			 f3 * (x2-x) * (y - y1)/((x2-x1)*(y2-y1)) +  f4 * (x-x1) * (y - y1)/((x2-x1)*(y2-y1))
	END SUBROUTINE

	! Checking if particle is on land and correct coordinates if land_mode=0
	SUBROUTINE is_on_land(long, lat, long_p, lat_p, id)
		real(8), intent(inout) :: long, lat
		real(8), intent(in) :: long_p, lat_p
		integer(4), intent(in) :: id
		real(8) long_m, lat_m

		if (p_nodes(id, 15) == 0) THEN
			if ((long < (p_nodes(id,3) + p_nodes(id,5))/2) .and. (lat > (p_nodes(id,4) + p_nodes(id,6))/2)) THEN
				if (LAND_MODE == 1) THEN
					flags(id) = -2
					print *, "WARNING: PARTICLE ", id," IS ON LAND"
				else
					long_m = (p_nodes(id,3) + p_nodes(id,5))/2
					lat_m = (p_nodes(id,4) + p_nodes(id,6))/2	
					if (long_p == long_m) THEN 
						long = long_m
					else if ((long_p > long_m) .and. (lat_p < lat_m)) THEN         
						if (p_nodes(id,16) < 0 .and. p_nodes(id,17) < 0) THEN  
							if (ABS(long - long_m) < ABS(lat - lat_m)) THEN
								long = long_m 			       
							else					       
								lat = lat_m                            
							end if
						else if ((p_nodes(id,16) > 0) .and. (p_nodes(id,17) > 0)) THEN
							long = long_m              
							lat =  lat_m 
						else if (p_nodes(id,16) > 0) THEN  
							lat = lat_m              
						else if (p_nodes(id,17) > 0) THEN 
							long = long_m               
						end if       
					else if (lat_p == lat_m) THEN
						lat = lat_m
					else if (long_p > long_m) THEN 
						long = long_m	 	
					else if (lat_p < lat_m) THEN  
						lat = lat_m 
					end if
				end if			
			end if
		end if
		if (p_nodes(id, 16) == 0) THEN
			if ((long > (p_nodes(id,3) + p_nodes(id,5))/2) .and. (lat > (p_nodes(id,4) + p_nodes(id,6))/2)) THEN
				if (LAND_MODE == 1) THEN
					flags(id) = -2
					print *, "WARNING: PARTICLE ", id," IS ON LAND"
				else
					long_m = (p_nodes(id,3) + p_nodes(id,5))/2
					lat_m = (p_nodes(id,4) + p_nodes(id,6))/2	
					if (long_p == long_m) THEN 
						long = long_m 
					else if ((long_p < long_m) .and. (lat_p < lat_m)) THEN         
						if (p_nodes(id,15) < 0 .and. p_nodes(id,18) < 0) THEN  
							if (ABS(long - long_m) < ABS(lat - lat_m)) THEN
								long = long_m 			       
							else					       
								lat = lat_m                            
							end if
						else if ((p_nodes(id,15) > 0) .and. (p_nodes(id,18) > 0)) THEN 
							long = long_m              
							lat =  lat_m 
						else if (p_nodes(id,15) > 0) THEN  
							lat = lat_m              
						else if  (p_nodes(id,18) > 0) THEN 
							long = long_m               
						end if      
					else if (lat_p == lat_m) THEN
						lat = lat_m
					else if (long_p < long_m) THEN 
						long = long_m	 	
					else if (lat_p < lat_m) THEN  
						lat = lat_m
					end if
				end if	 
			end if
		end if
		if (p_nodes(id, 17) == 0) THEN
			if ((long < (p_nodes(id,3) + p_nodes(id,5))/2) .and. (lat < (p_nodes(id,4) + p_nodes(id,6))/2)) THEN
				if (LAND_MODE == 1) THEN
					flags(id) = -2
					print *, "WARNING: PARTICLE ", id," IS ON LAND"
				else
					long_m = (p_nodes(id,3) + p_nodes(id,5))/2
					lat_m = (p_nodes(id,4) + p_nodes(id,6))/2	
					if (long_p == long_m) THEN 
						long = long_m
					else if ((long_p > long_m) .and. (lat_p > lat_m)) THEN         
						if (p_nodes(id,15) < 0 .and. p_nodes(id,18) < 0) THEN  
							if (ABS(long - long_m) < ABS(lat - lat_m)) THEN
								long = long_m 			       
							else					       
								lat = lat_m                            
							end if
						else if ((p_nodes(id,15) > 0) .and. (p_nodes(id,18) > 0)) THEN 
							long = long_m              
							lat =  lat_m 
						else if (p_nodes(id,15) > 0) THEN  
							long = long_m              
						else if  (p_nodes(id,18) > 0) THEN 
							lat =  lat_m               
						end if      
					else if (lat_p == lat_m) THEN
						lat = lat_m
					else if (long_p > long_m) THEN 
						long = long_m	 	
					else if (lat_p > lat_m) THEN  
						lat = lat_m          
					end if
				end if	
			end if
		end if
		if (p_nodes(id, 18) == 0) THEN
			if ((long > (p_nodes(id,3) + p_nodes(id,5))/2) .and. (lat < (p_nodes(id,4) + p_nodes(id,6))/2)) THEN
				if (LAND_MODE == 1) THEN
					flags(id) = -2
					print *, "WARNING: PARTICLE ", id," IS ON LAND"
				else
					long_m = (p_nodes(id,3) + p_nodes(id,5))/2
					lat_m = (p_nodes(id,4) + p_nodes(id,6))/2	
					if (long_p == long_m) THEN     !if previous longit coord = border
						long = long_m          !then longit=const
					else if (lat_p == lat_m) THEN  !else if latit coord = border
						lat = lat_m            !     latit= const
					else if ((long_p < long_m) .and. (lat_p > lat_m)) THEN         !if particle came from corner 1/4 cell
						if (p_nodes(id,17) < 0 .and. p_nodes(id,16) < 0) THEN  !if neigbours are sea 
							if (ABS(long - long_m) < ABS(lat - lat_m)) THEN!if particle came more on latit axis
								long = long_m 			       ! longit = const
							else					       ! else
								lat = lat_m                            ! latit = const
							end if
						else if ((p_nodes(id,16) > 0) .and. (p_nodes(id,17) > 0)) THEN !if both negbours - earth
							long = long_m              !longit=const and latit = const
							lat =  lat_m 
						else if (p_nodes(id,17) > 0) THEN  !if left neigbour is earth
							long = long_m              !longit=const
						else if  (p_nodes(id,16) > 0) THEN !if up neighbour us earth
							lat =  lat_m               !latit=const
						end if
					else if (long_p < long_m) THEN ! if previous longit coord less than
						long = long_m	       ! border, then particle ->]	
					else if (lat_p > lat_m) THEN   ! else ||
						lat = lat_m            !      \/
					end if
				end if	
			end if
		end if
	END SUBROUTINE

	!---------------------------------GET TRACKS-------------------------------------------------------!
	! Main cycle of getting tracks
	SUBROUTINE o_find_tracks
		do i = 1, TIMESTEPS
<<<<<<< HEAD
			if (mod(i, FREQUENCY_FOR_FILE) == 0) THEN
				call o_update(FREQUENCY_FOR_FILE)
=======
			call o_update(mod(i,FREQUENCY_FOR_FILE)+1)
			if (mod(i, FREQUENCY_FOR_FILE) == 0) THEN
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
				call o_gather_all_tracks
				call o_print_file_tracks(FREQUENCY_FOR_FILE)
				do n=1,N_P
					x1_p_track(n,0) = x1_p_track(n,FREQUENCY_FOR_FILE)
					x2_p_track(n,0) = x2_p_track(n,FREQUENCY_FOR_FILE)
					x1_p_track(n,1:FREQUENCY_FOR_FILE) = -1
<<<<<<< HEAD
					x2_p_track(n,1:FREQUENCY_FOR_FILE) = -1			
				end do
			else if (i == TIMESTEPS) THEN
				call o_update(mod(i,FREQUENCY_FOR_FILE))
				call o_gather_all_tracks
				call o_print_file_tracks(FREQUENCY_FOR_FILE)
			else
				call o_update(mod(i,FREQUENCY_FOR_FILE))
=======
					x2_p_track(n,1:FREQUENCY_FOR_FILE) = -1				
				end do
			else if (i == TIMESTEPS) THEN
				call o_gather_all_tracks
				call o_print_file_tracks(mod(TIMESTEPS,FREQUENCY_FOR_FILE))
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
			end if
		end do
	END SUBROUTINE

	! Get next location of particle
	SUBROUTINE o_update(step)
		integer, intent(in):: step
		real(8) u_c_inter, v_c_inter
		integer(4) subfield
		integer(4) id
		real(8) buffer(2*FREQUENCY_FOR_FILE + 3 + 4), recv_buffer(2*FREQUENCY_FOR_FILE + 3 + 4)  
		integer ierr
		integer req, stats(MPI_STATUS_SIZE)
		logical flag
		integer is_gone
		integer we, sn

		do n=1, N_P
			if (flags(n) == 1) then
				!call update_speeds(n)
				call is_on_land(x1_p(n), x2_p(n), x1_p_track(n,step-1),x2_p_track(n,step-1), n)
				call o_bilinear_interp(x1_p(n),x2_p(n),p_nodes(n,3), p_nodes(n,4), p_nodes(n,5), p_nodes(n,6), p_nodes(n,7), p_nodes(n,8),p_nodes(n,9),p_nodes(n,10),u_c_inter) 
				call o_bilinear_interp(x1_p(n),x2_p(n),p_nodes(n,3), p_nodes(n,4), p_nodes(n,5), p_nodes(n,6), p_nodes(n,11), p_nodes(n,12),p_nodes(n,13),p_nodes(n,14),v_c_inter) 
		
				x1_p_track(n,step) = x1_p(n)
				x2_p_track(n,step) = x2_p(n)
				
				x1_p(n) = x1_p(n) + (u_c_inter*DT*180.)/(RADIUS*PI*cos(x2_p(n)/180.*PI))
				x2_p(n) = x2_p(n) + (v_c_inter*DT*180.)/(RADIUS*PI)

				call is_in_cell(x1_p(n), x2_p(n), n, is_gone)
				if (is_gone == 1) THEN
					call is_in_subdomain(x1_p(n), x2_p(n), n)
					if (flags(n) == 0) then
						call choose_subdomain(x1_p(n), x2_p(n), subfield)
						if (subfield == -1) then
							print *, 'WARNING!!! PARTICLE ',n,' IS OUT OF BORDERS'
							flags(n) = -1
						else
							call choose_cell(x1_p(n), x2_p(n), n, we, sn)
							buffer(1:step) = x1_p_track(n,1:step)
							buffer(step+1:step*2) = x2_p_track(n,1:step)
							buffer(step*2+1) = DBLE(n)
							buffer(step*2+2) = x1_p(n)
							buffer(step*2+3) = x2_p(n)
							buffer(2*step+4) = p_nodes(n,1)
							buffer(2*step+5) = p_nodes(n,2) 
							buffer(2*step+6) = we
							buffer(2*step+7) = sn
<<<<<<< HEAD
							print *, "SEND"
=======
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
							call MPI_SEND(buffer,step*2+3+4 , MPI_DOUBLE_PRECISION, subfield,11,MY_COMM,req,ierr)
						end if
					else
						call choose_cell(x1_p(n), x2_p(n), n, we, sn)
						call set_cell_data(we, sn, n)
					end if
				end if
			end if
		end do
		call MPI_BARRIER(MY_COMM, ierr)
		flag = .true.
		do while (flag .eqv. .true.)
			call MPI_IRECV(recv_buffer, 2*step + 3 + 4, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, 11, MY_COMM, req, ierr)
			call MPI_TEST(req, flag, stats, ierr)
			if (flag .eqv. .true.) THEN
				id = recv_buffer(2*step+1)
				x1_p_track(id,1:step) = recv_buffer(1:step)
				x2_p_track(id,1:step) = recv_buffer(step+1:2*step)
				x1_p(id) = recv_buffer(2*step+2)
				x2_p(id) = recv_buffer(2*step+3)
				p_nodes(id,1) = recv_buffer(2*step+4)
				p_nodes(id,2) = recv_buffer(2*step+5)
				we = recv_buffer(2*step+6)
				sn = recv_buffer(2*step+7)
				call set_cell_data(we,sn,id)
				flags(id) = 1
			else
				call MPI_Cancel(req, ierr)
			end if
		end do
	END SUBROUTINE


	! Collect all tracks on processor with index MAIN_PROC
	SUBROUTINE o_gather_all_tracks
		integer ierr
		integer req, stats(MPI_STATUS_SIZE)
		logical flag
		real (8) :: sbuf(FREQUENCY_FOR_FILE * 2 + 1)
		real (8) :: rbuf(FREQUENCY_FOR_FILE * 2 + 1)

		if (rank == MAIN_PROC) THEN
			do j=1, N_P
				if (flags(j) == 1 .or. flags(j) == -1 .or. flags(j) == -2) THEN
					rbuf(2:FREQUENCY_FOR_FILE+1) = x1_p_track(j,1:FREQUENCY_FOR_FILE)
					rbuf(FREQUENCY_FOR_FILE+2:2*FREQUENCY_FOR_FILE+1) = x2_p_track(j,1:FREQUENCY_FOR_FILE)
					rbuf(1) = dble(j)
				else
<<<<<<< HEAD
					call MPI_RECV(rbuf, FREQUENCY_FOR_FILE*2+1, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, 12,MY_COMM,stats,ierr)
=======
					if (flags(j) == -3) THEN
						rbuf = -1
						rbuf(1) = j
					else
						print *, "recv"
						call MPI_RECV(rbuf, FREQUENCY_FOR_FILE*2+1, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, 12,MY_COMM,stats,ierr)
						print *, "got"
					end if
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
				end if
				collector((rbuf(1)-1)*(2*FREQUENCY_FOR_FILE+1)+1:rbuf(1)*(2*FREQUENCY_FOR_FILE + 1)) = rbuf
			end do
		end if
		do j = 1, N_P
			if (flags(j) == 1 .or. flags(j) == -1 .or. flags(j) == -2) THEN
				if (rank /= MAIN_PROC) THEN
					sbuf(2:FREQUENCY_FOR_FILE+1) = x1_p_track(j,1:FREQUENCY_FOR_FILE)
					sbuf(FREQUENCY_FOR_FILE+2:2*FREQUENCY_FOR_FILE+1) = x2_p_track(j,1:FREQUENCY_FOR_FILE)
					sbuf(1) = dble(j)
<<<<<<< HEAD
					!print *, "send"
=======
					print *, "send"
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
					call MPI_SEND(sbuf,FREQUENCY_FOR_FILE*2+1,MPI_DOUBLE_PRECISION, MAIN_PROC, 12, MY_COMM, req,ierr)
				end if
			end if
		end do
	END SUBROUTINE
END MODULE

PROGRAM main
	use shared_module
	use o_lagrange
	integer(4) :: ierr, rank_proc, count_processes
<<<<<<< HEAD
=======
	integer(4) :: Width = 11, Height = 11

>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
	double precision time_start, time_finish, tick
	
	call MPI_INIT(IERR)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, count_processes, ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, rank_proc, ierr)
	
<<<<<<< HEAD
	!print *, count_processes
=======
>>>>>>> 9c681a1b12b591f44908549114758bab43bfa8eb
	!ADJUSTMENT
	call o_set_config(count_processes, rank_proc)
	
	!ALLOCATION
	call o_alloc_data
	
	!INITIALISING	
	call o_ini_data

	!SOLVING
	tick = MPI_WTICK(ierr)
	time_start = MPI_WTIME(ierr)
	call o_find_tracks
	time_finish = MPI_WTIME(ierr)
	print *, 'rank=',rank_proc, 'time=',time_finish - time_start

	! PRINTING TO FILE		
	call o_print_file

	! DEALLOCATION
	call o_dealloc_data
	
	call MPI_FINALIZE(IERR)
END PROGRAM
