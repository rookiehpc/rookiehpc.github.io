#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to get the intersection of two groups of processes.
 * @details This code gets all processes of the default communicator and splits
 * them in two groups, designed to cover all cases: processes that belong to
 * both groups, one group or none.
 * It then gets the intersection of these two groups and creates a communicator
 * containing the processes of the intersection group. Each process then prints
 * whether it belongs to the communicator of the intersection group or not.
 * 
 * This application is meant to be run with 4 processes. The intersection can
 * be visualised as follows:
 *
 * +--------------+---+---+---+---+
 * | Processes    | 0 | 1 | 2 | 3 |
 * +--------------+---+---+---+---+
 * | Group A      | X |   | X |   |
 * | Group B      |   |   | X | X |
 * | Intersection |   |   | X |   |
 * +--------------+---+---+---+---+
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Check that the application is run with 4 processes.
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 4)
	{
		printf("Please run this application with 4 processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get the group from the default communicator
	MPI_Group world_group;
	MPI_Comm_group(MPI_COMM_WORLD, &world_group);

	// Keep the processes 0 and 2 in the group A
	MPI_Group group_a;
	int group_a_processes[2] = {0, 2};
	MPI_Group_incl(world_group, 2, group_a_processes, &group_a);

	// Keep the processes 2 and 3 in the group B
	MPI_Group group_b;
	int group_b_processes[2] = {2, 3};
	MPI_Group_incl(world_group, 2, group_b_processes, &group_b);

	// Get the intersection of both groups
	MPI_Group intersection_group;
	MPI_Group_intersection(group_a, group_b, &intersection_group);

	// Get my rank in the communicator
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Create a communicator made of the processes in the intersection group
	MPI_Comm new_communicator;
	MPI_Comm_create(MPI_COMM_WORLD, intersection_group, &new_communicator);
	if(new_communicator == MPI_COMM_NULL)
	{
		// I am not part of the communicator created, so I am not part of the intersection group
		printf("Process %d is not part of the intersection group.\n", my_rank);
	}
	else
	{
		// I am part of the communicator created, so I am part of the intersection group
		printf("Process %d is part of the intersection group.\n", my_rank);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}