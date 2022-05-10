#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use the MPI_Cart_shift routine.
 * @details This code creates a cartesian topology, then retrieves the rank of
 * up/down left/right neighbours via a shift.
 * For readability reasons, it is advised to run this code with 4 processes. The
 * toplogy created, given 4 processes, can be visualised as:
 *      +-----------+-----------+
 *      |           |           |
 *    ^ | process 1 | process 3 |
 *    | |           |           |
 * UP | +-----------+-----------+
 *    | |           |           |
 *    | | process 0 | process 2 |
 *      |           |           |
 *      +-----------------------+
 *        ------------------->
 *                RIGHT
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Size of the default communicator
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 4)
	{
		printf("This application is meant to be run with 4 processes, not %d.\n", size);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Ask MPI to decompose our processes in a 2D cartesian grid for us
	int dims[2] = {0, 0};
	MPI_Dims_create(size, 2, dims);

	// Make both dimensions non-periodic
	int periods[2] = {false, false};

	// Let MPI assign arbitrary ranks if it deems it necessary
	int reorder = true;

	// Create a communicator with a cartesian topology.
	MPI_Comm new_communicator;
	MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, reorder, &new_communicator);

	// Declare our neighbours
	enum DIRECTIONS {DOWN, UP, LEFT, RIGHT};
	char* neighbours_names[4] = {"down", "up", "left", "right"};
	int neighbours_ranks[4];

	// Let consider dims[0] = X, so the shift tells us our left and right neighbours
	MPI_Cart_shift(new_communicator, 0, 1, &neighbours_ranks[LEFT], &neighbours_ranks[RIGHT]);

	// Let consider dims[1] = Y, so the shift tells us our up and down neighbours
	MPI_Cart_shift(new_communicator, 1, 1, &neighbours_ranks[DOWN], &neighbours_ranks[UP]);

	// Get my rank in the new communicator
	int my_rank;
	MPI_Comm_rank(new_communicator, &my_rank);

	for(int i = 0; i < 4; i++)
	{
		if(neighbours_ranks[i] == MPI_PROC_NULL)
			printf("[MPI process %d] I have no %s neighbour.\n", my_rank, neighbours_names[i]);
		else
			printf("[MPI process %d] I have a %s neighbour: process %d.\n", my_rank, neighbours_names[i], neighbours_ranks[i]);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}