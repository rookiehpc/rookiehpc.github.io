#include <stdio.h>
#include <mpi.h>

/**
 * @brief Solution to the "Ordered hello world" MPI exercise.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);

	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	int i = 0;

	// I wait for everybody before me
	while(i < my_rank)
	{
		MPI_Barrier(MPI_COMM_WORLD);
		i++;
	}

	// I print my message
	printf("[MPI Process %d] Hello World!\n", my_rank);

	// I wait for everybody after me
	while(i < comm_size)
	{
		MPI_Barrier(MPI_COMM_WORLD);
		i++;
	}

	MPI_Finalize();

	return 0;
}