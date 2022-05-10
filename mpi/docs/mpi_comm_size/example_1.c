#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Display the number of MPI processes in the default communicator MPI_COMM_WORLD.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);

	if(my_rank == 0)
	{
		printf("There are %d MPI processes in the default communicator MPI_COMM_WORLD.\n", comm_size);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
