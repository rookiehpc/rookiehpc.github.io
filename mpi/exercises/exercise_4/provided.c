#include <stdio.h>
#include <mpi.h>

/**
 * @brief This exercise is to find the bug in a simple MPI hello world.
 **/
int main(int argc, char* argv[])
{
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);

	printf("\"Hello World!\" from MPI process %d. We are %d MPI processes.\n", my_rank, comm_size);

	MPI_Finalize();

	return 0;
}