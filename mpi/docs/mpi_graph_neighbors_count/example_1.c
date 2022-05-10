#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

/**
 * @brief Illustrates how to get the dimensions of a graph.
 * @details This application consists of 3 MPI processes that form a graph
 * that can be visualised as follows:
 *
 * +-----+              +-----+
 * |     |              |     |
 * |  0  |              |  1  |
 * |     |              |     |
 * +-----+              +-----+
 *  ^   |                    ^
 *  |   |                    |
 *  |   |    +-----+         |
 *  |   +--->|     |         |
 *  |        |  2  |         |
 *  +--------|     |---------+
 *           +-----+
 *
 * After creating the graph, each MPI process retrieves its number of neighbours
 * and prints it.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Size of the default communicator
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);

	if(comm_size != 3)
	{
		printf("This application is meant to be run with 3 MPI processes, not %d.\n", comm_size);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// My rank in the default communicator
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Declare the total number of neighbours until each MPI process (= the ones before + its own)
	int indexes[3] = {1, 1, 3};

	// Declare the endpoint of each edge
	int edges[3] = {2, 0, 1};

	// Allow MPI to reorder ranks if it deems it necessary
	int reorder = true;

	// Create a communicator given the graph topology.
	MPI_Comm new_communicator;
	MPI_Graph_create(MPI_COMM_WORLD, comm_size, indexes, edges, reorder, &new_communicator);

	// Get my number of neighbours and print it
	int number_of_neighbours_retrieved;
	MPI_Graph_neighbors_count(new_communicator, my_rank, &number_of_neighbours_retrieved);
	printf("[MPI process %d] I have %d neighbours.\n", my_rank, number_of_neighbours_retrieved);

	MPI_Finalize();

	return EXIT_SUCCESS;
}