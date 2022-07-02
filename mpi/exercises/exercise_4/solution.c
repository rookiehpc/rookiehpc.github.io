#include <stdio.h>
#include <mpi.h>

/**
 * @brief Solution to the incorrect hello world exercise.
 **/
int main(int argc, char* argv[])
{
	// ================================
	// => Add the MPI initialisation <=
	// ================================
	MPI_Init(&argc, &argv);

	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);

	printf("\"Hello World!\" from MPI process %d. We are %d MPI processes.\n", my_rank, comm_size);

	MPI_Finalize();

	return 0;
}