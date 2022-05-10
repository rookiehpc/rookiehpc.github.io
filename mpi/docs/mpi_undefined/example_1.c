#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to encounter a situation in which MPI_UNDEFINED is
 * returned.
 * @details This application is meant to be run with 4 processes. It creates a 
 * subgroup that will contain processes 0 and 1 only. Therefore, since processes
 * 2 and 3 do not belong to the subgroup, when they will try to get their rank
 * in that group, MPI_UNDEFINED will be returned.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Check that 4 MPI processes are used
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(comm_size != 4)
	{
		printf("This application is meant to be run with 4 MPI processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get the group or processes of the default communicator
	MPI_Group world_group;
	MPI_Comm_group(MPI_COMM_WORLD, &world_group);

	// Create the subgroup
	int ranks_subgroup[2] = {0, 1};
	MPI_Group subgroup;
	MPI_Group_incl(world_group, 2, ranks_subgroup, &subgroup);

	// Get my rank in the world group and the subgroup
	int my_world_group_rank;
	MPI_Group_rank(world_group, &my_world_group_rank);
	int my_subgroup_rank;
	MPI_Group_rank(subgroup, &my_subgroup_rank);

	if(my_subgroup_rank == MPI_UNDEFINED)
	{
		printf("I am process %d in world group but I am not part of the subgroup.\n", my_world_group_rank);
	}
	else
	{
		printf("I am process %d in world group and process %d in subgroup.\n", my_world_group_rank, my_subgroup_rank);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
