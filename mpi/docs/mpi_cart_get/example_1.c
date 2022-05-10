#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

/**
 * @brief Illustrates how to retrieve the cartesian topology information of a
 * communicator.
 * @details This code creates a new communicator with a cartesian topology. It
 * then retrieves the cartesian topology information and compares them with the
 * information given at creation.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Size of the default communicator
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	// Ask MPI to decompose our processes in a 2D cartesian grid for us
	int dimsGiven[2] = {0, 0};
	MPI_Dims_create(size, 2, dimsGiven);

	// Make both dimensions periodic
	int periodsGiven[2] = {true, false};

	// Let MPI assign arbitrary ranks if it deems it necessary
	int reorderGiven = true;

	// Create a communicator given the 2D torus topology.
	MPI_Comm new_communicator;
	MPI_Cart_create(MPI_COMM_WORLD, 2, dimsGiven, periodsGiven, reorderGiven, &new_communicator);

	// Retrieve cartesian topology information
	int dimsRetrieved[2];
	int periodsRetrieved[2];
	int my_coords[2];
	MPI_Cart_get(new_communicator, 2, dimsRetrieved, periodsRetrieved, my_coords);

	// Master prints a comparison between dims / periods given and retrieved.
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	if(my_rank == 0)
	{
		printf("Dims given (%d, %d) vs (%d, %d) retrieved.\n", dimsGiven[0], dimsGiven[1], dimsRetrieved[0], dimsRetrieved[1]);
		printf("Periods given (%d, %d) vs (%d, %d) retrieved.\n", periodsGiven[0], periodsGiven[1], periodsRetrieved[0], periodsRetrieved[1]);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}