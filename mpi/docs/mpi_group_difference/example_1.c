#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to create a group by difference.
 * @details This application is meant to be run with 4 processes. It creates two 
 * groups, named A and B, which contain processes 0,1 and 1,3 respectively. It
 * then creates a group that is the difference of groups A and B (all processes
 * belong to the first group that are not in the second one). It can be 
 * visualised as follows:
 * 
 *                    +---+---+---+---+
 *                    | 0 | 1 | 2 | 3 |
 * +------------------+---+---+---+---+
 * | Group A          | X | X |   |   |
 * | Group B          |   | X |   | X |
 * | Difference group | X |   |   |   |
 * +------------------+---+---+---+---+
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

	// Create the group A
	int ranks_group_a[2] = {0, 1};
	MPI_Group group_a;
	MPI_Group_incl(world_group, 2, ranks_group_a, &group_a);

	// Create the group B
	int ranks_group_b[2] = {1, 3};
	MPI_Group group_b;
	MPI_Group_incl(world_group, 2, ranks_group_b, &group_b);

	// Create the difference of groups A and B
	MPI_Group difference_group;
	MPI_Group_difference(group_a, group_b, &difference_group);

	// Get my rank in the world group and the difference group
	int my_world_group_rank;
	MPI_Group_rank(world_group, &my_world_group_rank);
	int my_difference_group_rank;
	MPI_Group_rank(difference_group, &my_difference_group_rank);

	if(my_difference_group_rank == MPI_UNDEFINED)
	{
		printf("I am process %d in world group but I am not part of the difference group.\n", my_world_group_rank);
	}
	else
	{
		printf("I am process %d in world group and process %d in difference group.\n", my_world_group_rank, my_difference_group_rank);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
