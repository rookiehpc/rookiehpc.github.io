#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use a reduce scatter.
 * @details This application is meant to be run with 3 MPI processes. It
 * consists of a sum reduction; every MPI process has four values to send for
 * reduction. The first values from the MPI process will be reduced and stored 
 * on the MPI process 0. The second and third values will be reduced separately 
 * and stored on MPI process 1, similarly with the fourth values on MPI process
 * 2. It can be visualised as follows:
 *
 *      +---------------+  +---------------+  +---------------+
 *      |   Process 0   |  |   Process 1   |  |   Process 2   |
 *      +---------------+  +---------------+  +---------------+
 *      |     Values    |  |     Values    |  |     Values    |
 *      +---+---+---+---+  +---+---+---+---+  +---+---+---+---+
 *      | 0 | 1 | 2 | 3 |  | 4 | 5 | 6 | 7 |  | 8 | 9 | 10| 11|
 *      +---+---+---+---+  +---+---+---+---+  +---+---+---+---+
 *        |    \   \   \     /  |     |  \      /   /   /   |
 *        | ____\___\___\___/___|_____|___\____/   /   /    |
 *        |/     \   \   \      |     |    \      /   /     |
 *        |       \___\___\____ | ____|_____\____/   /      |
 *        |            \   \   \|/    |      \      /       |
 *        |             \___\___|____ | ______\____/        |
 *        |                  \  |    \|/       \            |
 *        |                   \_|_____|_________\__________ |
 *        |                     |     |                    \|
 *        |                     |     |                     |
 *     +--+--+                +-+---+-+---+              +--+--+
 *     | SUM |                | SUM | SUM |              | SUM |
 *     +-----+                +-----+-----+              +-----+
 *     |  12 |                |  15 |  18 |              |  21 |
 *     +--+--+                +--+--+--+--+              +--+--+
 *        |                      |     |                    |      
 *  +-----+-----+             +--+-----+--+           +-----+-----+
 *  | Process 0 |             | Process 1 |           | Process 2 |
 *  +-----------+             +-----------+           +-----------+
 *  |   Value   |             |   Values  |           |   Value   |
 *  +-----------+             +-----+-----+           +-----------+
 *  |     13    |             |  16 |  20 |           |     23    |
 *  +-----------+             +-----+-----+           +-----------+
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get the size of the communicator
	int size = 0;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 3)
	{
		printf("This application is meant to be run with 3 MPI processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Defines my values
	int values[4] = {4 * my_rank, 4 * my_rank + 1, 4 * my_rank + 2, 4 * my_rank + 3};

	// Define the block lengths
	int counts[3] = {1, 2, 1};

	if(my_rank == 1)
	{
		// Each MPI process sends its values and the buffer to receive the corresponding reduction results
		int reduction_results[2];
		MPI_Reduce_scatter(values, reduction_results, counts, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
		printf("[MPI process %d] The sum I received are %d and %d.\n", my_rank, reduction_results[0], reduction_results[1]);
	}
	else
	{
		// Each MPI process sends its values and the buffer to receive the corresponding reduction results
		int reduction_result;
		MPI_Reduce_scatter(values, &reduction_result, counts, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
		printf("[MPI process %d] The sum I received is %d.\n", my_rank, reduction_result);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
