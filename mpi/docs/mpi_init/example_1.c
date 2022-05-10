#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to initialise the MPI environment.
 **/
int main(int argc, char* argv[])
{
	// Initilialise MPI and check its completion
	MPI_Init(&argc, &argv);

	// Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	printf("Process %d has initialised its MPI environment.\n", my_rank);

	// Tell MPI to shut down.
	MPI_Finalize();

	return EXIT_SUCCESS;
}