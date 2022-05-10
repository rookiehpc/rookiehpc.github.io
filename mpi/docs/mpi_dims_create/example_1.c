#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to decompose MPI processes in a cartesian grid.
 * @details This application is to be run with 60 processes. It attempts to
 * decompose these 60 MPI processes in 3 dimensions (i.e: a cube).
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get total number of MPI processes
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 12)
	{
		printf("This application is meant to be run with 12 processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get my MPI process identifier
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	if(my_rank == 0)
	{
		printf("Attempting to decompose %d processes into a cube:\n", size);

		// Give total freedom to MPI
		int dimsA[3] = {0, 0, 0};
		printf("\t- Restrictions (%d, %d, %d) give decomposition ", dimsA[0], dimsA[1], dimsA[2]);
		MPI_Dims_create(size, 3, dimsA);
		printf("(%d, %d, %d).\n", dimsA[0], dimsA[1], dimsA[2]);

		// Restrict 6 processes in the first dimension
		int dimsB[3] = {6, 0, 0};
		printf("\t- Restrictions (%d, %d, %d) give decomposition ", dimsB[0], dimsB[1], dimsB[2]);
		MPI_Dims_create(size, 3, dimsB);
		printf("(%d, %d, %d).\n", dimsB[0], dimsB[1], dimsB[2]);

		// Restrict 4 processes in the second dimension and 1 in the third one
		int dimsC[3] = {0, 4, 1};
		printf("\t- Restrictions (%d, %d, %d) give decomposition ", dimsC[0], dimsC[1], dimsC[2]);
		MPI_Dims_create(size, 3, dimsC);
		printf("(%d, %d, %d).\n", dimsC[0], dimsC[1], dimsC[2]);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
