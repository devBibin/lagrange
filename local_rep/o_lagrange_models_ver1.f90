!-----------------------------------------------------------------------------------------------------
!Author: Bibin Vladimir
!Contact: devBibinVA@gmail.com
!Set particle's coordinates in SUBROUTINE o_fill_particles 
!Used file descriptor: 11

! Implementation of lagrange particle transport
MODULE o_lagrange
	use o_basic_module
	use o_grid_module
	use shared_module
	use comp_module
	implicit none
	save 	
	integer :: AllocateStatus, n, irecvrequest 				! Indexes and statuses	
	integer :: i_west, j_south, i_east, j_north, lagrange_rank		! Subfield's indexes
	real(8),allocatable ::  x1_p (:), x2_p (:)				! Particles' current coordinate (1 - Width, 2 - height)
	real(8),allocatable :: x1_p_track (:,:), x2_p_track(:,:)		! Particles' trajectory
	real(8),allocatable :: p_nodes(:,:)					! cell particle info
	integer,allocatable :: flags(:)						! 0 - out, 1 - in, -1 - left map, -2 - on land, 2 - sending
	integer :: westrank, eastrank, southrank, northrank			! Frame neigbours indexes of processor. 
	integer :: serank, swrank, nerank, nwrank				! -1 - no neighbour
	real (8), allocatable :: collector(:)					! Collector of tracks (only on main processor)
	! Constants (see set_default constants)
	real(4) :: DT								! Timestep
	integer :: MAIN_PROC							! Index of Main Processor
	integer :: FREQUENCY_FOR_FILE						! Write to file/iterations
	integer :: MY_COMM							! Communicator for MPI
	integer :: DEEP								! Grid parameters (size of map) 	
	integer :: N_P				 				! Number of particles
	integer :: TIMESTEPS							! Count of timesteps
	integer :: LAND_MODE							! 0 - drifting near coast, 1 - particle stops on coast
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
	SUBROUTINE allocate_lagrange_module
		! Set constants for allocation
		call set_default_constants

		! Allocation
		ALLOCATE ( x1_p(N_P), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x1_p = -999
		ALLOCATE ( x2_p(N_P), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x2_p = -999
		ALLOCATE ( flags(N_P), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); flags = -1
		ALLOCATE ( x1_p_track(N_P, 0:TIMESTEPS), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x1_p_track = -1
		ALLOCATE ( x2_p_track(N_P, 0:TIMESTEPS), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); x2_p_track = -1
		if (lagrange_rank == MAIN_PROC) THEN
			ALLOCATE (collector(N_P*(2*TIMESTEPS + 1)), STAT = AllocateStatus)
			call o_alloc_check(AllocateStatus); collector = -999
		end if
		ALLOCATE (p_nodes(N_P, 18), STAT = AllocateStatus)
		call o_alloc_check(AllocateStatus); p_nodes = -1
	END SUBROUTINE

	! Deallocate arrays
	SUBROUTINE o_dealloc_data
		 DEALLOCATE ( x1_p, STAT = AllocateStatus)
		 DEALLOCATE ( x2_p, STAT = AllocateStatus)
		 DEALLOCATE ( x1_p_track, STAT = AllocateStatus)
		 DEALLOCATE ( x2_p_track, STAT = AllocateStatus)
		 DEALLOCATE ( flags, STAT = AllocateStatus)
 		 DEALLOCATE ( p_nodes, STAT = AllocateStatus)
 		 if (lagrange_rank == MAIN_PROC) THEN
			DEALLOCATE (collector, STAT = AllocateStatus)
		 end if
	END SUBROUTINE



	!--------------------------CONFIGURE SECTION--------------------------------!
	! Set essential model's constants
	SUBROUTINE set_default_constants
		N_P = 10000	 		!number of particles
		MAIN_PROC = 0			!index of main process, which gather info
		MY_COMM = comm_local		!communicator of module
		TIMESTEPS = 10000		!count of TIMESTEPS in model
		DT = 10000			!timestep
		DEEP = 1			!deep of slice
		FREQUENCY_FOR_FILE = 100 	!write to file once per 10 iterations
		LAND_MODE = 0			!0- if particle is drifting near the coast, 1 - if particle stays on land
		call o_set_indexes(jsouth(myrank), jnorth(myrank), iwest(myrank), ieast(myrank), rank_local)
		call o_set_neigbours(comp_westrank, comp_eastrank, comp_southrank, comp_northrank,comp_serank, comp_swrank, comp_nerank, comp_nwrank) 
		open(11,FILE='tracks/tracks.txt')
	END SUBROUTINE

	! Set grid indexes for each processor
	SUBROUTINE o_set_indexes(south_j, north_j, west_i, east_i, rank_proc)
		integer, intent(in) :: south_j, north_j, west_i, east_i, rank_proc
		i_west = west_i - 1
		j_south = south_j - 1
	   	i_east = east_i
		j_north = north_j
		lagrange_rank = rank_proc
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
	! Fill particles coordinates
	SUBROUTINE o_set_particles_coordinates
		integer :: x1_root, x2_root
		x1_p = 135.26
		x2_p = 77.45
		!x1_p(201:500) = 139.4
		!x2_p(201:500) = 74.79
		!x1_p(501:1000) = 129.2
		!x2_p(501:1000) = 71.5
		!x1_p(1001:1200) = 139.62
		!x2_p(1001:1200) = 72
		!x1_p(1201:1400) = 128.6
		!x2_p(1401:1500) = 71.88
		!x1_p(1501:1700) = 134.2
		!x2_p(1501:1700) = 71.5
		!x1_p(1701:2000) = 123.123
		!x2_p(1701:2000) = 73.04
		
		do i=1,N_P
			call is_in_subdomain(x1_p(i), x2_p(i), i)
			if (flags(i) == 1) THEN
				call dihotomic(x1_p(i), x2_p(i), x1_root, x2_root)
				p_nodes(i, 1) = x1_root
				p_nodes(i, 2) = x2_root
				call set_cell_data(0, 0, i)			
			end if
		end do
	END SUBROUTINE


	!-------------------------------PRINT SECTION-------------------------------------------!
	SUBROUTINE o_print_file
		character(len=1024) :: filename
		write (filename, "(A,I4,A)") "tracks/greed_speed", lagrange_rank, ".txt"
		!PRINT TO FILE GRID AND SPEED
		do j = j_south, j_north, 1
			do i = i_west, i_east, 1
				open(13,FILE=filename)
				write (13,'(f10.6,A,f10.6,A,f10.6,A,f10.6,A,I4)'), x1_v(i,j),';', x2_v(i,j),';', u_c(1, i, j),';',& 
				v_c(1,i,j),';', kv(i,j)
			end do
		end do

		! PRINT TO FILE SUBFIELDS INFO
		write (filename, "(A,I4,A)") "tracks/subfields", lagrange_rank, ".txt"		
		open (12, file=filename)
		write (12,'(I2, A, I4, A, I4, A, I4, A, I4)'), lagrange_rank,';', i_west,';', j_south,';', i_east,';', j_north
		CLOSE (12, STATUS='KEEP')
		CLOSE (13, STATUS='KEEP')  
	END SUBROUTINE

	! Print information about each node to file (Width,height)
	SUBROUTINE o_print_file_tracks(col_count)
		integer, intent(in) :: col_count
		integer :: ind 
		! PRINT TO FILE TRACKS
		if (lagrange_rank == MAIN_PROC) THEN
			j = 1
			do while (j <= N_P * (2*col_count + 1))
				if (mod(j, 2*col_count+1) == 1) THEN
					ind = collector(j)
				else
					if (collector(j) /= -1) THEN
						write (11, '(I10,A,f15.6,A,f15.6)'),ind,';',collector(j),';', collector(j+col_count)
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
		p_nodes(id, 15) = kv(x1_root, x2_root)
		p_nodes(id, 16) = kv(x1_root+1, x2_root)
		p_nodes(id, 17) = kv(x1_root, x2_root-1)
		p_nodes(id, 18) = kv(x1_root+1, x2_root-1)
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

	! Defining subdomain where particle left for
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
		!print *, "LEFT FOR", subfield
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
					if ((long_p >= long_m) .and. (lat_p <= lat_m)) THEN         
						if (p_nodes(id,16) > 0 .and. p_nodes(id,17) > 0) THEN  
							if (ABS(long - long_m) < ABS(lat - lat_m)) THEN
								long = long_m 			       
							else					       
								lat = lat_m                            
							end if
						else if ((p_nodes(id,16) == 0) .and. (p_nodes(id,17) == 0)) THEN
							long = long_m              
							lat =  lat_m 
						else if (p_nodes(id,16) == 0) THEN  
							lat = lat_m              
						else if (p_nodes(id,17) == 0) THEN 
							long = long_m               
						end if
					else if (long_p == long_m) THEN 
						long = long_m       
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
					if ((long_p <= long_m) .and. (lat_p <= lat_m)) THEN         
						if (p_nodes(id,15) > 0 .and. p_nodes(id,18) > 0) THEN  
							if (ABS(long - long_m) < ABS(lat - lat_m)) THEN
								long = long_m 			       
							else					       
								lat = lat_m                            
							end if
						else if ((p_nodes(id,15) == 0) .and. (p_nodes(id,18) == 0)) THEN 
							long = long_m              
							lat =  lat_m 
						else if (p_nodes(id,15) == 0) THEN  
							lat = lat_m              
						else if  (p_nodes(id,18) == 0) THEN 
							long = long_m               
						end if
					else if (long_p == long_m) THEN 
						long = long_m       
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
					if ((long_p >= long_m) .and. (lat_p >= lat_m)) THEN         
						if (p_nodes(id,15) > 0 .and. p_nodes(id,18) > 0) THEN  
							if (ABS(long - long_m) < ABS(lat - lat_m)) THEN
								long = long_m 			       
							else					       
								lat = lat_m                            
							end if
						else if ((p_nodes(id,15) == 0) .and. (p_nodes(id,18) == 0)) THEN 
							long = long_m              
							lat =  lat_m 
						else if (p_nodes(id,15) == 0) THEN  
							long = long_m              
						else if  (p_nodes(id,18) == 0) THEN 
							lat =  lat_m               
						end if
					else if (long_p == long_m) THEN 
						long = long_m      
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
					if ((long_p <= long_m) .and. (lat_p >= lat_m)) THEN         !if particle came from corner 1/4 cell
						if (p_nodes(id,17) > 0 .and. p_nodes(id,16) > 0) THEN  !if neigbours are sea 
							if (ABS(long - long_m) < ABS(lat - lat_m)) THEN!if particle came more on latit axis
								long = long_m 			       ! longit = const
							else					       ! else
								lat = lat_m                            ! latit = const
							end if
						else if ((p_nodes(id,16) == 0) .and. (p_nodes(id,17) == 0)) THEN !if both negbours - earth
							long = long_m              !longit=const and latit = const
							lat =  lat_m 
						else if (p_nodes(id,17) == 0) THEN  !if left neigbour is earth
							long = long_m              !longit=const
						else if  (p_nodes(id,16) == 0) THEN !if up neighbour us earth
							lat =  lat_m               !latit=const
						end if
					else if (long_p == long_m) THEN!if previous longit coord = border
						long = long_m          !then longit=const
					else if (lat_p == lat_m) THEN  !else if latit coord = border
						lat = lat_m            !     latit= const
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
	SUBROUTINE o_find_tracks(i)
		integer, intent(in) :: i
		call o_update(i)
		if (mod(i, FREQUENCY_FOR_FILE) == 0) THEN
			call o_gather_all_tracks(i-FREQUENCY_FOR_FILE+1,FREQUENCY_FOR_FILE)
			call o_print_file_tracks(FREQUENCY_FOR_FILE)
			call o_print_file
		else if (i == TIMESTEPS) THEN
			call o_gather_all_tracks(TIMESTEPS - mod(TIMESTEPS,FREQUENCY_FOR_FILE) + 1, mod(TIMESTEPS,FREQUENCY_FOR_FILE))
			call o_print_file_tracks(mod(TIMESTEPS,FREQUENCY_FOR_FILE))
		end if
	END SUBROUTINE

	! Get next location of particle
	SUBROUTINE o_update(step)
		integer, intent(in):: step
		real(8) u_c_inter, v_c_inter
		integer(4) subfield
		integer(4) id
		real(8) buffer(2*TIMESTEPS + 3 + 4), recv_buffer(2*TIMESTEPS + 3 + 4)  
		integer ierr
		integer req, stats(MPI_STATUS_SIZE)
		logical flag
		integer is_gone
		integer we, sn

		do n=1, N_P
			if (flags(n) == 1) then
				call update_speeds(n)
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
							print *, 'WARNING!!! PARTICLE ',n,' IS OUT OF BORDERS ON ITERATION', step
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
							call MPI_SEND(buffer,step*2+3+4 , MPI_DOUBLE_PRECISION, subfield,771,MY_COMM,req,ierr)
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
			call MPI_IRECV(recv_buffer, 2*step + 3 + 4, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, 771, MY_COMM, req, ierr)
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
	SUBROUTINE o_gather_all_tracks(start_ind, arr_count)
		integer, intent(in) :: start_ind, arr_count
		integer ierr
		integer req, stats(MPI_STATUS_SIZE)
		logical flag
		real (8) :: sbuf(arr_count * 2 + 1)
		real (8) :: rbuf(arr_count * 2 + 1)

		if (lagrange_rank == MAIN_PROC) THEN
			do j=1, N_P
				if (flags(j) == 1 .or. flags(j) == -1 .or. flags(j) == -2) THEN
					rbuf(2:arr_count+1) = x1_p_track(j,start_ind:start_ind+arr_count-1)
					rbuf(arr_count+2:2*arr_count+1) = x2_p_track(j,start_ind:start_ind+arr_count-1)
					rbuf(1) = dble(j)
				else
					call MPI_RECV(rbuf, arr_count*2+1, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, 1277,MY_COMM,stats,ierr)
				end if
				collector((rbuf(1)-1)*(2*arr_count+1)+1:rbuf(1)*(2*arr_count + 1)) = rbuf
			end do
		end if
		do j = 1, N_P
			if (flags(j) == 1 .or. flags(j) == -1 .or. flags(j) == -2) THEN
				if (lagrange_rank /= MAIN_PROC) THEN
					sbuf(2:arr_count+1) = x1_p_track(j,start_ind:start_ind+arr_count-1)
					sbuf(arr_count+2:2*arr_count+1) = x2_p_track(j,start_ind:start_ind+arr_count-1)
					sbuf(1) = dble(j)
					call MPI_SEND(sbuf,arr_count*2+1,MPI_DOUBLE_PRECISION, MAIN_PROC, 1277, MY_COMM, req,ierr)
				end if
			end if
		end do
	END SUBROUTINE
END MODULE
