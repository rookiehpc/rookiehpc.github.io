#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to put data into a target window.
 * @details This application consists of two MPI processes. MPI process 1
 * exposes a window containing an integer. MPI process 0 puts the value 12345
 * in it via MPI_Put. After the MPI_Put is issued, synchronisation takes place
 * via MPI_Win_fence and the MPI process 1 prints the value in its window.
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
	int window_buffer = 0;
	MPI_Win window;
	MPI_Win_create(&window_buffer, sizeof(int), sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &window);
	if(my_rank == 1)
	{
		printf("[MPI process 1] Value in my window_buffer before MPI_Put: %d.\n", window_buffer);
	}
	MPI_Win_fence(0, window);

	if(my_rank == 0)
	{
		// Push my value into the first integer in MPI process 1 window
		int my_value = 12345;
		MPI_Put(&my_value, 1, MPI_INT, 1, 0, 1, MPI_INT, window);
		printf("[MPI process 0] I put data %d in MPI process 1 window via MPI_Put.\n", my_value);
	}

	// Wait for the MPI_Put issued to complete before going any further
	MPI_Win_fence(0, window);

	if(my_rank == 1)
	{
		printf("[MPI process 1] Value in my window_buffer after MPI_Put: %d.\n", window_buffer);
	}

	// Destroy the window
	MPI_Win_free(&window);

	MPI_Finalize();

	return EXIT_SUCCESS;
}