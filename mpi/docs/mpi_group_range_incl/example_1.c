#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to create a group by including ranges of processes
 * from another group.
 * @details This application is meant to be run with 4 processes. It begins with
 * the default group made of all processes and then creates another group from
 * it, keeping only odd process ranks.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Check that 4 processes are used
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(comm_size != 4)
	{
		printf("This application is meant to be run with 4 processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get the group or processes of the default communicator
	MPI_Group world_group;
	MPI_Comm_group(MPI_COMM_WORLD, &world_group);

	// Get my rank in the world group
	int my_world_group_rank;
	MPI_Group_rank(world_group, &my_world_group_rank);

	// Selecting odd ranks means selecting every 2 ranks starting from 1, until the max rank <comm_size - 1>.
	int rank_ranges[1][3] = {1, comm_size-1, 2};
	MPI_Group odd_group;
	MPI_Group_range_incl(world_group, 1, rank_ranges, &odd_group);

	// Get my rank in the odd group
	int my_odd_group_rank;
	MPI_Group_rank(odd_group, &my_odd_group_rank);

	// Continue only if we are part of the odd group
	if(my_odd_group_rank != MPI_UNDEFINED)
	{
		printf("I am process %d in world group and %d in the odd group.\n", my_world_group_rank, my_odd_group_rank);
	}
	else
	{
		printf("I am process %d in world group but I am not part of the odd group.\n", my_world_group_rank);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
