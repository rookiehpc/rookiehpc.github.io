#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to create a shared window.
 * @details This application consists in creating a shared window and interact
 * with it using direct memory accesses. Two MPI processes are used, each will
 * hold an integer initialised to 100. MPI process 0 will increment MPI process
 * 1's variable, and MPI process 1 will decrement MPI process 0's variable.
 * In this application, the overall shared window uses the default configuration
 * where it is made of contiguous data.
 *
 * This can be visualised as follows:
 *
 * - Start situation:
 *         Held on MPI process 0 | Held on MPI process 1
 *                         +-----+-----+
 *                         | 100 | 100 |
 *                         +-----+-----+
 *         My element = array[0] | My element = array[0]
 *       Peer element = array[1] | Peer element = array[-1]
 *
 * - End situation:
 *         Held on MPI process 0 | Held on MPI process 1
 *                         +-----+-----+
 *                         |  99 | 101 |
 *                         +-----+-----+
 *         My element = array[0] | My element = array[0]
 *       Peer element = array[1] | Peer element = array[-1]
 *
 * This code assumes MPI processes must be able to physically share memory.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Check that only 2 MPI processes are spawn
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(comm_size != 2)
	{
		printf("This application is meant to be run with 2 MPI processes, not %d.\n", comm_size);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Create the window
	const int ARRAY_SIZE = 1;
	int* window_buffer;
	MPI_Win window;
	MPI_Win_allocate_shared(ARRAY_SIZE * sizeof(int), sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &window_buffer, &window);
	printf("[MPI process %d] Window created.\n", my_rank);

	// Initialise my element
	*window_buffer = 100;
	printf("[MPI process %d] Value before direct write from MPI process %d: %d.\n", my_rank, comm_size - 1 - my_rank, *window_buffer);

	// Modify peer's element
	MPI_Barrier(MPI_COMM_WORLD);
	if(my_rank == 0)
	{
		window_buffer[1]++;
	}
	else
	{
		window_buffer[-1]--;
	}
	MPI_Barrier(MPI_COMM_WORLD);

	// Check end values
	printf("[MPI process %d] Value after direct write from MPI process %d: %d.\n", my_rank, comm_size - 1 - my_rank, *window_buffer);

	// Destroy the window
	printf("[MPI process %d] Window destroyed.\n", my_rank);
	MPI_Win_free(&window);

	MPI_Finalize();

	return EXIT_SUCCESS;
}