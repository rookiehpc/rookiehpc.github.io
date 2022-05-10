#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to create a window with an automatically allocated
 * memory area.
 * @details This application consists in creating a window and destroying it.
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
	const int ARRAY_SIZE = 2;
	int* window_buffer;
	MPI_Win window;
	MPI_Win_allocate(ARRAY_SIZE * sizeof(int), sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &window_buffer, &window);
	printf("[MPI process %d] Window created.\n", my_rank);

	// Issue RMA communications
	// ...
	// Wait for all RMA communications to complete

	// Destroy the window
	printf("[MPI process %d] Window destroyed.\n", my_rank);
	MPI_Win_free(&window);

	MPI_Finalize();

	return EXIT_SUCCESS;
}