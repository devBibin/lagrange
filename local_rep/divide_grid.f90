!Primitive module for grid divide
MODULE divide_grid
	implicit none
	integer(4) :: proc_count = 0
	contains

SUBROUTINE g_fill_neigbours(comp_westrank, comp_eastrank, comp_southrank, comp_northrank,comp_serank, comp_swrank, comp_nerank, comp_nwrank)
		integer, intent(out) :: comp_westrank, comp_eastrank, comp_southrank, comp_northrank
		integer, intent(out) :: comp_serank, comp_swrank, comp_nerank, comp_nwrank
		comp_westrank = rank - 1 
		comp_eastrank = rank + 1
		comp_southrank = rank - proc_count_axis
		comp_northrank = rank + proc_count_axis
		comp_serank =  rank - proc_count_axis + 1
		comp_swrank = rank - proc_count_axis - 1
		comp_nerank = rank + proc_count_axis + 1
		comp_nwrank = rank + proc_count_axis - 1
		if (mod(rank, proc_count_axis) == 0) then
			comp_southrank = -1			
			comp_swrank = -1
			comp_serank = -1
		endif
		if (mod(rank, proc_count_axis) == proc_count_axis-1) then
			comp_northrank = -1			
			comp_nwrank = -1
			comp_nerank = -1
		endif
		if (rank / proc_count_axis == 0)  then
			comp_westrank = -1
			comp_nwrank = -1
			comp_swrank = -1
		endif
		if (rank / proc_count_axis == proc_count_axis_axis-1)  then
			comp_eastrank = -1
			comp_nerank = -1
			comp_serank = -1
		endif		
END SUBROUTINE
	! Get indexes of process with defined rank
	SUBROUTINE g_fill_indexes(rank, w_min, w_max, h_min, h_max, j_south, j_north, i_west, i_east)
		integer, intent(out) :: j_south, j_north, i_west, i_east
		integer(4) :: step_w, step_h
		integer(4) :: i, j

		step_w = (w_max-w_min)/proc_count
		step_h = (h_max-h_min)/proc_count

		j_south = 1 + mod(rank, proc_count) * step_h 
		j_north = 1 + (mod(rank, proc_count) + 1) * step_h
		i_west = 1 + rank / proc_count * step_w
		i_east = 1 + (rank / proc_count + 1) * step_w
		
		!if (rank == 0) then
		!	j_south = 1 + mod(rank, proc_count) * step_h 
		!	j_north = 1 + (mod(rank, proc_count) + 1) * step_h
		!	i_west = 1 + rank / proc_count * step_w
		!	i_east = 1 + (rank / proc_count + 1) * step_w
		!else if (mod(rank, proc_count) == 0) then
		!	j_south = 1 + mod(rank, proc_count) * step_h 
		!	j_north = 1 + (mod(rank, proc_count) + 1) * step_h
		!	i_west = 2 + rank / proc_count * step_w
		!	i_east = 1 + (rank / proc_count + 1) * step_w
		!else if (rank / proc_count == 0)  then
		!	j_south = 2 + mod(rank, proc_count) * step_h 
		!	j_north = 1 + (mod(rank, proc_count) + 1) * step_h
		!	i_west = 1 + rank / proc_count * step_w
		!	i_east = 1 + (rank / proc_count + 1) * step_w
		!else
		!	j_south = 2 + mod(rank, proc_count) * step_h 
		!	j_north = 1 + (mod(rank, proc_count) + 1) * step_h
		!	i_west = 2 + rank / proc_count * step_w
		!	i_east = 1 + (rank / proc_count + 1) * step_w
		!endif		
	END SUBROUTINE

	! Checking if count of processors is convenient for our grid	
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
