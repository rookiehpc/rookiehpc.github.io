#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use an MPI_Op handle.
 * @details This application consists of multiple reductions applied in turn.
 * An array of MPI_Op handles contains the reduction operations to run. All MPI
 * processes perform an MPI reduction for each of the operations held in that
 * array: a sum, a product and a max.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get the number of processes and check only 4 are used.
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 4)
	{
		printf("This application is meant to be run with 4 processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Determine root's rank
	int root_rank = 0;

	// Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Store the operations to run
	const int OPERATION_COUNT = 3;
	MPI_Op operations[OPERATION_COUNT] = { MPI_SUM, MPI_PROD, MPI_MAX };
	char* operations_label[OPERATION_COUNT] = { "sum", "prod", "max" };

	// Each MPI process sends its rank to reduction, root MPI process collects the result
	int reduction_result;

	for(int i = 0; i < OPERATION_COUNT; i++)
	{
		reduction_result = 0;
		MPI_Reduce(&my_rank, &reduction_result, 1, MPI_INT, operations[i], root_rank, MPI_COMM_WORLD);

		if(my_rank == root_rank)
		{
			printf("The %s of all ranks is %d.\n", operations_label[i], reduction_result);
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
