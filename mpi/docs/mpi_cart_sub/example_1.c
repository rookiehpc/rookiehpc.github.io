#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

/**
 * @brief Illustrates how to partition a cartesian topology created with
 * MPI_Cart_create.
 * @details This program is meant to be run with 6 MPI processes. It consists in
 * partitioning a 2 x 3 2D cartesian topology along its first dimension to
 * obtain 2 1D subgrids of 3 MPI processes each.
 * The initial 2D cartesian topology can be visualised as follows:
 *
 *                  Second dimension
 * ------------------------------------------------->
 *
 * +---------------+---------------+---------------+   |
 * | MPI process 0 | MPI process 1 | MPI process 2 |   |
 * |     (0,0)     |     (0,1)     |     (0,2)     |   |
 * +---------------+---------------+---------------+   |  First dimension
 * | MPI process 3 | MPI process 4 | MPI process 5 |   |
 * |     (1,0)     |     (1,1)     |     (1,2)     |   |
 * +---------------+---------------+---------------+   v
 *
 * And the final subgrids can be visualised as follows:
 *
 * +---------------+---------------+---------------+  \
 * | MPI process 0 | MPI process 1 | MPI process 2 |   } First subgrid
 * +---------------+---------------+---------------+  /
 *
 * +---------------+---------------+---------------+  \
 * | MPI process 3 | MPI process 4 | MPI process 5 |   } Second subgrid
 * +---------------+---------------+---------------+  /
 *
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Size of the default communicator
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	if(size != 6)
	{
		printf("This application is meant to be run with 6 processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Ask MPI to decompose our processes in a 2D cartesian grid for us
	int dims[2] = {2, 3};

	// Make both dimensions non-periodic
	int periods[2] = {false, false};

	// Let MPI assign arbitrary ranks if it deems it necessary
	int reorder = true;

	// Create a communicator given the 2D torus topology.
	MPI_Comm cartesian_communicator;
	MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, reorder, &cartesian_communicator);

	// My rank in the new communicator
	int my_rank;
	MPI_Comm_rank(cartesian_communicator, &my_rank);

	// Get my coordinates in the new communicator
	int my_coords[2];
	MPI_Cart_coords(cartesian_communicator, my_rank, 2, my_coords);

	// Print my location in the 2D cartesian topology.
	printf("[MPI process %d] I am located at (%d, %d) in the initial 2D cartesian topology.\n", my_rank, my_coords[0], my_coords[1]);

	// Partition the 2D cartesian topology along the first dimension, by preserving the second dimension
	int remain_dims[2] = {false, true};
	MPI_Comm subgrid_communicator;
	MPI_Cart_sub(cartesian_communicator, remain_dims, &subgrid_communicator);

	// Get the ranks of all MPI processes in my subgrid and print it
	int subgrid_ranks[3];
	MPI_Allgather(&my_rank, 1, MPI_INT, subgrid_ranks, 1, MPI_INT, subgrid_communicator);
	printf("[MPI process %d] I am in the 1D subgrid that contains MPI processes %d, %d and %d.\n", my_rank, subgrid_ranks[0], subgrid_ranks[1], subgrid_ranks[2]);

	MPI_Finalize();

	return EXIT_SUCCESS;
}