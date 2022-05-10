#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to abort all processes in the MPI_COMM_WORLD
 * communicator.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
