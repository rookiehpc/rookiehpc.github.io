#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

/**
 * @brief Illustrates how to get the indexes and edges of a graph.
 * @details This application consists of 3 MPI processes that form a fully
 * connected graph that can be visualised as follows:
 *
 * +-----+              +-----+
 * |     |------------->|     |
 * |  0  |              |  1  |
 * |     |<-------------|     |
 * +-----+              +-----+
 *  ^   |                |   ^
 *  |   |                |   |
 *  |   |    +-----+     |   |
 *  |   +--->|     |<----+   |
 *  |        |  2  |         |
 *  +--------|     |---------+
 *           +-----+
 *
 * After creating the graph, each MPI process retrieves the graph indexes and
 * edges, and prints them.
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
	int indexes[3] = {2, 4, 6};

	// Declare the endpoint of each edge
	int edges[6] = {1, 2, 0, 2, 0, 1};

	// Allow MPI to reorder ranks if it deems it necessary
	int reorder = true;

	// Create a communicator given the graph topology.
	MPI_Comm new_communicator;
	MPI_Graph_create(MPI_COMM_WORLD, comm_size, indexes, edges, reorder, &new_communicator);

	// Get the graph dimensions
	int number_of_indexes_retrieved;
	int number_of_edges_retrieved;
	MPI_Graphdims_get(new_communicator, &number_of_indexes_retrieved, &number_of_edges_retrieved);

	// Retrieve the indexes and edges
	int* indexes_retrieved = (int*) malloc(sizeof(int) * number_of_indexes_retrieved);
	int* edges_retrieved = (int*) malloc(sizeof(int) * number_of_edges_retrieved);
	MPI_Graph_get(new_communicator, number_of_indexes_retrieved, number_of_edges_retrieved, indexes_retrieved, edges_retrieved);

	// Print all information retrieved
	printf("[MPI process %d] %d indexes retrieved: {", my_rank, number_of_indexes_retrieved);
	for(int i = 0; i < number_of_indexes_retrieved; i++)
	{
		printf("%d", indexes_retrieved[i]);
		if(i < number_of_indexes_retrieved - 1)
		{
			printf(", ");
		}
	}
	printf("}, and %d edges retrieved: {", number_of_edges_retrieved);
	for(int i = 0; i < number_of_edges_retrieved; i++)
	{
		printf("%d", edges_retrieved[i]);
		if(i < number_of_edges_retrieved - 1)
		{
			printf(", ");
		}
	}
	printf("}.\n");

	MPI_Finalize();

	return EXIT_SUCCESS;
}