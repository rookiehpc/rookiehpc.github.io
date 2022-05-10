#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to check if a group is empty.
 * @details This application is meant to be run with 4 processes. It creates two 
 * disjoint groups, named A and B, which contain processes 0,2 and 1,3
 * respectively. It then creates a group that is the intersection of groups A
 * and B. Therefore, the final group created is guaranteed to be empty, which is
 * what we need here to illustrate the MPI_GROUP_EMPTY constant.
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
	int ranks_group_a[2] = {0, 2};
	MPI_Group group_a;
	MPI_Group_incl(world_group, 2, ranks_group_a, &group_a);

	// Create the group B
	int ranks_group_b[2] = {1, 3};
	MPI_Group group_b;
	MPI_Group_incl(world_group, 2, ranks_group_b, &group_b);

	// Create the intersection of groups A and B
	MPI_Group group_intersection;
	MPI_Group_intersection(group_a, group_b, &group_intersection);

	if(group_intersection == MPI_GROUP_EMPTY)
	{
		printf("The intersection group created is empty.\n");
	}
	else
	{
		printf("The intersection group created is not empty.\n");
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
