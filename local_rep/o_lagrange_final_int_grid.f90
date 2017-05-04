! Module for using MPI
MODULE shared_module
	implicit none
	include 'mpif.h'
END MODULE shared_module

! Primitive module for grid divide
MODULE divide_grid
	use shared_module
	integer(4) :: proc_count_axis = 0
	integer(4) :: rank
	contains
	! Get frame processors indexes
	SUBROUTINE g_get_neigbours(westrank, eastrank, southrank, northrank, serank, swrank, nerank, nwrank)
		integer(4), intent(out):: westrank, eastrank, southrank, northrank
		integer(4), intent(out):: serank, swrank, nerank, nwrank
		westrank = rank - proc_count_axis
		eastrank = rank + proc_count_axis
		southrank = rank - 1
		northrank = rank + 1
		serank =  rank + proc_count_axis - 1
		swrank = rank - proc_count_axis - 1
		nerank = rank + proc_count_axis + 1
		nwrank = rank - proc_count_axis + 1
		if (mod(rank, proc_count_axis) == 0) then
			southrank = -1			
			swrank = -1
			serank = -1
		endif
		if (mod(rank, proc_count_axis) == proc_count_axis-1) then
			northrank = -1			
			nwrank = -1
			nerank = -1
		endif
		if (rank / proc_count_axis == 0)  then
			westrank = -1
			nwrank = -1
			swrank = -1
		endif
		if (rank / proc_count_axis == proc_count_axis-1)  then
			eastrank = -1
			nerank = -1
			serank = -1
		endif			
	END SUBROUTINE

	! Get grid indexes of process with defined rank
	SUBROUTINE g_get_indexes(rank_proc, w_min, w_max, h_min, h_max, j_south, j_north, i_west, i_east)
		integer, intent(in) :: w_min, w_max, h_min, h_max, rank_proc
		integer, intent(out) :: j_south, j_north, i_west, i_east
		integer(4) :: step_w, step_h
		integer(4) :: i, j

		rank = rank_proc
		step_w = (w_max-w_min)/proc_count_axis
		step_h = (h_max-h_min)/proc_count_axis

		j_south = 1 + mod(rank, proc_count_axis) * step_h 
		j_north = 1 + (mod(rank, proc_count_axis) + 1) * step_h
		i_west = 1 + rank / proc_count_axis * step_w
		i_east = 1 + (rank / proc_count_axis + 1) * step_w		
	END SUBROUTINE

	! Checking if count of processors is convenient for our grid	
	SUBROUTINE g_axis_count_proc(proc_count_all)
		integer(4), intent(in) :: proc_count_all 
		integer(4) :: i		
		do i = 1,10,1
			if (i*i == proc_count_all) THEN
				proc_count_axis = i
			endif
		enddo
		if (proc_count_axis == 0) THEN
			print *, 'enter integer square digit as count of proc. Default: grid with 1 kernel'
			proc_count_axis = 1
		endif
		if (proc_count_all > 100) THEN
			print *, 'enter digit <= 100 as count of proc. Default: grid with 1 kernel'
			proc_count_axis = 1
		endif
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
	integer(4) :: Deep, jl , il						! Grid parameters	
	integer(4) :: n_p, timesteps		 				! Number of particles	
	real(4), parameter :: radius = 6378100.0, pi = 3.141592653589793238	! Standart constants
	integer(4) :: AllocateStatus, i, j, n					! Indexes and statuses	
	integer(4) :: i_west, j_south, i_east, j_north, rank			! Subfield's indexes
	real(8),allocatable :: x1_v (:,:), x2_v(:,:) 				! Coordinates of the grid (1 - width, 2 - height)
	real(8),allocatable :: u_c (:,:,:), v_c(:,:,:)				! Speeds in roots of the grid
	real(8),allocatable ::  x1_p (:), x2_p (:)				! Particles' current coordinate (1 - width, 2 - height)
	real(8), allocatable :: x1_p_track (:,:), x2_p_track(:,:)		! Particles' trajectory
	real(4) :: dt								! Time step
	integer(4), allocatable :: flags(:)
	integer :: westrank, eastrank, southrank, northrank
	integer :: serank, swrank, nerank, nwrank
	integer :: count_proc
	real (8), allocatable :: collector(:)
	integer(4) :: MAIN_PROC
contains
	!-----------------MEMORY SECTION----------------------!
	! Checking allocation
	SUBROUTINE o_alloc_check(stat)
		integer(4), intent(in) :: stat
		IF (stat /= 0) THEN
			PRINT *, 'Not enough memory'			
			STOP
		END IF
	END SUBROUTINE

	! Allocate arrays 
	SUBROUTINE o_alloc_data
		ALLOCATE ( x1_v(i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)		
		ALLOCATE ( x2_v(i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)		
		ALLOCATE ( u_c(Deep,i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)		
		ALLOCATE ( v_c(Deep,i_west:i_east, j_south:j_north), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus)
		ALLOCATE ( x1_p(n_p), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x1_p = -999
		ALLOCATE ( x2_p(n_p), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x2_p = -999
		ALLOCATE ( flags(n_p), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); flags = 0
		ALLOCATE ( x1_p_track(n_p, timesteps), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x1_p_track = -1
		ALLOCATE ( x2_p_track(n_p, timesteps), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x2_p_track = -1
		if (rank == MAIN_PROC) THEN
			ALLOCATE (collector(n_p*(2*timesteps + 1)), STAT = AllocateStatus)
			call o_alloc_check(AllocateStatus); collector = -999
		end if
	END SUBROUTINE

	! Deallocate arrays
	SUBROUTINE o_dealloc_data
		 DEALLOCATE ( x1_v, STAT = AllocateStatus)
		 DEALLOCATE ( x2_v, STAT = AllocateStatus)
		 DEALLOCATE ( u_c, STAT = AllocateStatus)
		 DEALLOCATE ( v_c, STAT = AllocateStatus)
	END SUBROUTINE



	!--------------------------CONFIGURE SECTION--------------------------------!
	! Set essential model's constants
	SUBROUTINE o_set_default_constants
		n_p = 3
		MAIN_PROC = 0
		timesteps = 100
		dt = 1
		Deep = 1
		il = 11
		jl = 11
	END SUBROUTINE

	! Set size of our grid
	SUBROUTINE o_set_size(wi, he)
		integer, intent(in) :: wi, he
		il = wi
		jl = he
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
				x1_v(i, j) = 0. + real(i)
				x2_v(i, j) = 0. + real(j)
			end do
		end do
	END SUBROUTINE

	! Calculate anticyclone and fill vectors' speed field
	SUBROUTINE o_fill_u_c
		real(4) :: wc, hc, rmax, x, y, r, u, v, cos_alfa, sin_alfa

		wc = (il-1)/2.0
		hc = (jl-1)/2.0

		rmax = sqrt( wc**2 + hc**2)

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

	! Fill particles coordinates
	SUBROUTINE o_fill_particles
		x1_p = 7
		x2_p = 4

		x1_p(2) = 5
		x2_p(2) = 1

		x1_p(3) = 2
		x2_p(3) = 7


		do i=1,n_p
			call is_in_field(x1_p(i), x2_p(i), i)
		end do
	END SUBROUTINE


	!-------------------------------PRINT SECTION-------------------------------------------!
	! Print grid and field of speed vectors on the screen
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

	! Print inforamation about each node to file (width,height)
	SUBROUTINE o_print_file
		character(len=1024) :: filename
		write (filename, "(A,I4,A)") "files_ver1/greed_speed", rank, ".txt"
		!PRINT TO FILE GRID AND SPEED
		do j = j_south, j_north, 1
			do i = i_west, i_east, 1
				open(13,FILE=filename)
				write (13,'(f10.6,A,f10.6,A,f10.6,A,f10.6)'), x1_v(i,j),';', x2_v(i,j),';', u_c(1, i, j),';',& 
				v_c(1,i,j)
			end do
		end do
		! PRINT TO FILE TRACKS
		open(12,FILE='files_ver1/tracks.txt')
		if (rank == MAIN_PROC) THEN
			i = 1
			do while (i <= n_p * (2*timesteps + 1))
				if (mod(i, 2*timesteps+1) == 1) THEN
					ind = collector(i)
				else
					if (collector(i) /= -1) THEN
						write (12, '(I4,A,f15.6,A,f15.6)'),ind,';',collector(i),';', collector(i+timesteps)
					end if
				end if
				if (mod(i,((ind-1)*(2*timesteps+1) + timesteps + 1)) == 0) THEN
					i = i + timesteps + 1
				else
					i = i + 1 
				end if
			end do
		end if

		! PRINT TO FILE SUBFIELDS INFO
		write (filename, "(A,I4,A)") "files_ver1/subfields", rank, ".txt"		
		open (11, file=filename)
		write (11,'(I2, A, I4, A, I4, A, I4, A, I4)'), rank,';', i_west,';', j_south,';', i_east,';', j_north
	END SUBROUTINE



	!-----------------------------------MAIN SECTION------------------------------------------------------!
	!-----------------------------------AUXILARY FUNCTIONS------------------------------------------------!
	! Check if particle in processor's area
	SUBROUTINE is_in_field(w,h, id)
		integer(4), intent(in) :: id
		real(8),intent(in)  :: w, h		
		integer(4):: w_int, h_int
		call get_nearest_node(w, w_int)
		call get_nearest_node(h, h_int)
		if ((w_int >= i_west) .and. (w_int < i_east) .and. (h_int >= j_south) .and. (h_int < j_north)) THEN
			flags(id) = 1
		else
			flags(id) = 0
		end if
	END SUBROUTINE

	! Defining area where particle left for
	SUBROUTINE choose_subfield(w, h, subfield)
		integer(4), intent(out) :: subfield
		real(8),intent(in)  :: w, h		
		integer(4):: w_int, h_int
		call get_nearest_node(w, w_int)
		call get_nearest_node(h, h_int)
		if (w_int < i_west) THEN
			if (h_int >= j_north) THEN
				subfield = nwrank
			else if (h_int < j_south) THEN
				subfield = swrank
			else
				subfield = westrank			
			end if
		else if (w_int >= i_east) THEN
			if (h_int >= j_north) THEN
				subfield = nerank
			else if (h_int < j_south) THEN
				subfield = serank
			else
				subfield = eastrank		
			end if
		else
			if (h_int >= j_north) THEN
				subfield = northrank
			else if (h_int < j_south) THEN
				subfield = southrank
			end if
		end if
	END SUBROUTINE

	! Get nearest node to particle's location
	SUBROUTINE get_nearest_node(x, res)
		real(8), intent(in) :: x
		integer(4), intent(out) :: res
		res = FLOOR(x)
	END SUBROUTINE

	! bilinear interpolation
	! x, y - coordinates of the particle
	! x1, y1 - left bottom node
	! x2, y2 - right upper node
	! f1, f2, f3, f4 - left-up, right-up, left-down, right-down
	SUBROUTINE o_bilinear_interp(x, y, x1,y1,x2,y2,f3,f4,f1,f2, res)
		integer(4), intent(in) :: x1, x2, y1, y2
		real(8), intent(in) :: x, y
		real(4), intent(in) :: f1, f2, f3, f4
		real(4), intent(out) :: res
		res = f1 * (x2-x) * (y2 - y)/((x2-x1)*(y2-y1)) +  f2 * (x-x1) * (y2 - y)/((x2-x1)*(y2-y1)) +&
			 f3 * (x2-x) * (y - y1)/((x2-x1)*(y2-y1)) +  f4 * (x-x1) * (y - y1)/((x2-x1)*(y2-y1))
	END SUBROUTINE


	!---------------------------------GET TRACKS-------------------------------------------------------!
	! Main cycle of getting tracks
	SUBROUTINE o_find_tracks
		do i = 1, timesteps
			call o_update(i)
		end do
		call o_gather_all
	END SUBROUTINE

	! Get next location of particle
	SUBROUTINE o_update(step)
		integer(4), intent(in):: step
		real(4) u_c_inter, v_c_inter
		real(4) u(4), v(4)
		integer(4) x1, x2, x3, x4
		integer(4) y1, y2, y3, y4
		integer(4) ip(n_p), jp(n_p)
		integer(4) subfield
		integer(4) id
		integer(4) recv_step
		real(8) buffer(2*timesteps + 3), recv_buffer(2*timesteps + 3)  
		integer ierr
		integer req, stats(MPI_STATUS_SIZE)
		logical flag

		print *, rank,'|', step
		
		recv_step = step - 1
		flag = .true.
		do while (flag .eqv. .true.)
			call MPI_IRECV(recv_buffer, 2*recv_step + 3, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, step, MPI_COMM_WORLD, req, ierr)
			call MPI_TEST(req, flag, stats, ierr)
			if (flag .eqv. .true.) THEN
				print *, "recieved particle=",recv_buffer(2*recv_step+1),"from rank=",stats(MPI_SOURCE),"in rank=",rank
				print *, "current coordinates=",recv_buffer(2*recv_step+2), recv_buffer(2*recv_step+3)
				id = recv_buffer(2*recv_step+1)
				x1_p_track(id,1:recv_step) = recv_buffer(1:recv_step)
				x2_p_track(id,1:recv_step) = recv_buffer(recv_step+1:2*recv_step)
				x1_p(id) = recv_buffer(2*recv_step+2)
				x2_p(id) = recv_buffer(2*recv_step+3)
				flags(id) = 1
			end if
		end do

		do n=1, n_p
			if (flags(n) == 1) then
				call get_nearest_node(x1_p(n), x1)
				call get_nearest_node(x2_p(n), y1)
				x2 = x1 + 1
				y2 = y1
				x3 = x1
				y3 = y1 + 1
				x4 = x1 + 1
				y4 = y1 + 1
				
				u(1) = u_c(DEEP,x1  ,y1)
				u(2) = u_c(DEEP,x2  ,y2)
				u(3) = u_c(DEEP,x3  ,y3)
				u(4) = u_c(DEEP,x4  ,y4)

				v(1) = v_c(DEEP,x1  ,y1)
				v(2) = v_c(DEEP,x2  ,y2)
				v(3) = v_c(DEEP,x3  ,y3)
				v(4) = v_c(DEEP,x4  ,y4)

				call o_bilinear_interp(x1_p(n),x2_p(n),x3,y3,x2,y2,u(1),u(2),u(3),u(4),u_c_inter) 
		    		call o_bilinear_interp(x1_p(n),x2_p(n),x3,y3,x2,y2,v(1),v(2),v(3),v(4),v_c_inter)	
		
				x1_p_track(n,step) = x1_p(n)
				x2_p_track(n,step) = x2_p(n)
	
				x1_p(n) = x1_p(n) + u_c_inter*dt!(u_c_inter*dt*180.)/(radius*pi*cos(x2_p(n)/180.*pi))
				x2_p(n) = x2_p(n) + v_c_inter*dt!(v_c_inter*dt*180.)/(radius*pi)

				call is_in_field(x1_p(n), x2_p(n), n)
			
				if (flags(n) == 0) then
					call choose_subfield(x1_p(n), x2_p(n), subfield)
					if (subfield == -1) then
						print *, 'WARNING!!! PARTICLE ',n,' IS OUT OF BORDERS'
						print *, x1_p(n),x2_p(n)
						flags(n) = -1
					else
						print *, 'SEND TO ', subfield, 'Particle=', n
						buffer(1:step) = x1_p_track(n,1:step)
						buffer(step+1:step*2) = x2_p_track(n,1:step)
						buffer(step*2+1) = DBLE(n)
						buffer(step*2+2) = x1_p(n)
						buffer(step*2+3) = x2_p(n)	
						call MPI_SEND(buffer,step*2+3 , MPI_DOUBLE_PRECISION, subfield, step+1,MPI_COMM_WORLD,req,ierr)
					end if
				end if
			end if
		end do
		!call sleep(1)
		call MPI_BARRIER(MPI_COMM_WORLD, ierr)
	END SUBROUTINE



	SUBROUTINE o_gather_all
		integer ierr
		integer req, stats(MPI_STATUS_SIZE)
		logical flag
		real (8) :: sbuf(timesteps * 2 + 1)
		real (8) :: rbuf(timesteps * 2 + 1)

		if (rank == MAIN_PROC) THEN
			do i=1, n_p
				if (flags(i) == 1 .or. flags(i) == -1) THEN
					rbuf(2:timesteps+1) = x1_p_track(i,1:timesteps)
					rbuf(timesteps+2:2*timesteps+1) = x2_p_track(i,1:timesteps)
					rbuf(1) = dble(i)
				else
					call MPI_RECV(rbuf, timesteps*2+1, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, timesteps+1,MPI_COMM_WORLD,stats,ierr)
				end if
				collector((i-1)*(2*timesteps+1)+1:i*(2*timesteps + 1)) = rbuf
			end do
		end if

		do i = 1, n_p
			if (flags(i) == 1 .or. flags(i) == -1) THEN
				if (rank /= MAIN_PROC) THEN
					sbuf(2:timesteps+1) = x1_p_track(i,1:timesteps)
					sbuf(timesteps+2:2*timesteps+1) = x2_p_track(i,1:timesteps)
					sbuf(1) = dble(i)
					call MPI_SEND(sbuf,timesteps*2+1,MPI_DOUBLE_PRECISION, MAIN_PROC, timesteps+1, MPI_COMM_WORLD, req,ierr)
				end if
			end if
		end do
	END SUBROUTINE
END MODULE

PROGRAM main
	use shared_module
	use o_lagrange
	use divide_grid
	integer(4) :: ierr, rank_proc, count_processes
	integer(4) :: Width = 11, Height = 11
	integer(4) :: south, north, west, east
	integer(4) :: r_s, r_n, r_w, r_e, r_se, r_sw, r_ne, r_nw

	
	call MPI_INIT(IERR)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, count_processes, ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, rank_proc, ierr)
	
	!GET GRID INFORMATION FOR EACH PROCESS	
	call g_axis_count_proc(count_processes)	
	call g_get_indexes(rank_proc, 1, Height, 1, Width, south, north, west, east)
	call g_get_neigbours(r_w, r_e, r_s, r_n, r_se, r_sw, r_ne, r_nw)
	
	!ADJUSTMENT
	call o_set_default_constants
	call o_set_size(Width, Height)	
	call o_set_indexes(south,north,west,east,rank_proc, count_processes)
	call o_set_neigbours(r_w, r_e, r_s, r_n, r_se, r_sw, r_ne, r_nw)
	
	!ALLOCATION
	call o_alloc_data
	
	!INITIALISING	
	call o_ini_data

	!SOLVING
	call o_find_tracks
	
	! PRINTING TO FILE		
	call o_print_file

	! DEALLOCATION
	!call o_dealloc_data

	call MPI_FINALIZE(IERR)
END PROGRAM
