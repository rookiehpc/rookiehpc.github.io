#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to finalise the MPI environment.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Tell MPI to shut down and that we no longer need it.
	MPI_Finalize();

	return EXIT_SUCCESS;
}