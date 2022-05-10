#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

/**
 * @brief Illustrates how to print a process location in a cartesian grid.
 * @details This code creates a new communicator given a 2D torus topology. It
 * then makes each process print its coordinates in the 2D torus communicator.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Size of the default communicator
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	// Ask MPI to decompose our processes in a 2D cartesian grid for us
	int dims[2] = {0, 0};
	MPI_Dims_create(size, 2, dims);

	// Make both dimensions periodic
	int periods[2] = {true, true};

	// Let MPI assign arbitrary ranks if it deems it necessary
	int reorder = true;

	// Create a communicator given the 2D torus topology.
	MPI_Comm new_communicator;
	MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, reorder, &new_communicator);

	// My rank in the new communicator
	int my_rank;
	MPI_Comm_rank(new_communicator, &my_rank);

	// Get my coordinates in the new communicator
	int my_coords[2];
	MPI_Cart_coords(new_communicator, my_rank, 2, my_coords);

	// Print my location in the 2D torus.
	printf("[MPI process %d] I am located at (%d, %d).\n", my_rank, my_coords[0],my_coords[1]);

	MPI_Finalize();

	return EXIT_SUCCESS;
}