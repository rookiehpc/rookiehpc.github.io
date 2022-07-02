#include <stdio.h>
#include <mpi.h>

/**
 * @brief Solution to the distributed sum MPI exercise.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Initialise my reduction variable
	int my_value = my_rank * 100;
	printf("Value held by MPI process %d: %d.\n", my_rank, my_value);

	// Perform the reduction
	int total;
	MPI_Reduce(&my_value, &total, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

	// If I am MPI process 0, I also pass a variable to store the reduction result
	if(my_rank == 0)
	{
		printf("Total sum reduced at MPI process %d: %d.\n", my_rank, total);
	}

	MPI_Finalize();

	return 0;
}