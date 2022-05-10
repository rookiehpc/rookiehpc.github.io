#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

/**
 * @brief Illustrates how to check whether a process is part of a new
 * communicator.
 * @details This code creates a new communicator from a cartesian grid designed
 * to contain only 1 process. By running this application with multiple
 * processes, all but one will not belong to the new communicator created.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Size of the default communicator
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	// Rank in the default communicator
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Use a cartesian grid of 1 process in total
	int dims[2] = {1, 1};

	// Make both dimensions periodic
	int periods[2] = {true, true};

	// Let MPI assign arbitrary ranks if it deems it necessary
	int reorder = true;

	// Create a communicator given the 2D torus topology.
	MPI_Comm new_communicator;
	MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, reorder, &new_communicator);

	// Check if I am part of the new communicator
	if(new_communicator == MPI_COMM_NULL)
	{
		printf("Process %d is not part of the new communicator.\n", my_rank);
	}
	else
	{
		printf("Process %d is part of the new communicator.\n", my_rank);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}