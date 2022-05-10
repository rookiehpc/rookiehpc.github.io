#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to create a group by including processes from another
 * group.
 * @details This application is meant to be run with 4 processes. It begins with
 * the default group made of all processes and then creates another group from
 * it, keeping only processes 1 and 3.
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

	// Create the small group by including only processes 1 and 3 from the world group
	int small_group_ranks[2] = {1, 3};
	MPI_Group small_group;
	MPI_Group_incl(world_group, 2, small_group_ranks, &small_group);

	// Get my rank in the small group
	int my_small_group_rank;
	MPI_Group_rank(small_group, &my_small_group_rank);

	// Continue only if we are part of the small group
	if(my_small_group_rank != MPI_UNDEFINED)
	{
		printf("I am process %d in world group and %d in small group.\n", my_world_group_rank, my_small_group_rank);
	}
	else
	{
		printf("I am process %d in world group but I am not part of the small group.\n", my_world_group_rank);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
