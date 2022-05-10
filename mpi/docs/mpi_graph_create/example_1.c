#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

/**
 * @brief Illustrates how to create a communicator representing a graph
 * topology.
 * @details This application consists of 7 MPI processes that will be rearranged
 * into an undirected graph (i.e: non-symmetric), before they check if they 
 * belong to this graph, and if so, print their number of neighbours. To
 * simplify, the ranks of the MPI processes in the default communicator
 * MPI_COMM_WORLD are preserved in the graph.
 *
 * The topology created can be visualised as follows:
 *
 * +-----+              +-----+                +-----+
 * |     |              |     |<---------------|     |
 * |  0  |------------->|  1  |                |  3  |
 * |     |              |     |--------------->|     |
 * +-----+              +-----+                +-----+
 *    \                    ^
 *     \                  /
 *      \                /
 *       \              /                   +-----+
 *        \            /                    |     |
 *         \          /                     |  5  |<-----+
 *          \        /                      |     |      |
 *           \      /                       +-----+      |
 *            \    /                           |         |
 *             v  /                            +---------+
 *           +-----+           +-----+
 *           |     |           |     |
 *           |  2  |           |  4  |
 *           |     |           |     |
 *           +-----+           +-----+
 *
 * This graph topology above was designed so that all cases are covered:
 * - Node connecting to a single neighbour (MPI process 1, 2, 3)
 * - Node connecting to multiple neighbours (MPI process 0)
 * - Node connected by a single neighbour (MPI processes 2, 3)
 * - Node connected by multiple neighbours (MPI process 1)
 * - Nodes forming a cycle (MPI processes 1 and 3, MPI process 5)
 * - Node without neighbours (MPI process 4)
 * - Node connected to itself (MPI Process 5)
 * - MPI process not included at all in the graph (MPI process 6)
 *
 *
 * The adjacency matrix of the graph topology above is given in the table below:
 *
 * +---------+-----------+--------+-----------+
 * |   MPI   | Connected | Degree | Cumulated |
 * | process |     to    |        |  degree   |
 * +---------+-----------+--------+-----------+
 * |    0    | 1, 2      |    2   |      2    |
 * |    1    | 3         |    1   |      3    |
 * |    2    | 1         |    1   |      4    |
 * |    3    | 1         |    1   |      5    |
 * |    4    | -         |    0   |      5    |
 * |    5    | 5         |    1   |      6    |
 * +---------+-----------+--------+-----------+
 *
 * The agglomerated list containing the MPI ranks of all neighbours can be
 * visualised as follows:
 *
 *         +------------------ End of MPI process 0's neighbours
 *         |   +-------------- End of MPI process 1's neighbours
 *         |   |   +---------- End of MPI process 2's neighbours
 *         |   |   |   +------ End of MPI process 3's neighbours
 *         |   |   |   |
 *         |   |   |   +------ End of MPI process 4's neighbours
 *         |   |   |   |       (It is also the end of MPI process 3's neighbours,
 *         |   |   |   |        meaning that MPI process 4 has no neighbours)
 *         |   |   |   |   +-- End of MPI process 5's neighbours
 *         |   |   |   |   |
 *         v   v   v   v   v
 * +---+---+---+---+---+---+
 * | 1 | 2 | 3 | 1 | 1 | 5 |
 * +---+---+---+---+---+---+
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Size of the default communicator
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	if(size != 7)
	{
		printf("This application is meant to be run with 7 MPI processes, not %d.\n", size);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// My rank in the default communicator
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Number of nodes in the graph, the MPI process 4 will not be connected to
	// anyone so it is not included in the graph.
	const int number_of_nodes = 6;

	// Declare the total number of neighbours until each MPI process (= the ones before + its own)
	int indexes[number_of_nodes] = {2, 3, 4, 5, 5, 6};

	// Declare the endpoint of each edge
	int edges[6] = {1, 2, 3, 1, 1, 5};

	// Let preserve the original MPI ranks to simplify
	int reorder = false;

	// Create a communicator given the graph topology.
	MPI_Comm new_communicator;
	MPI_Graph_create(MPI_COMM_WORLD, number_of_nodes, indexes, edges, reorder, &new_communicator);

	if(new_communicator != MPI_COMM_NULL)
	{
		int my_number_of_neighbours;
		MPI_Graph_neighbors_count(new_communicator, my_rank, &my_number_of_neighbours);
		printf("[MPI process %d] I am part of the graph and have %d neighbours.\n", my_rank, my_number_of_neighbours);
	}
	else
	{
		printf("[MPI process %d] I am not part of the graph communicator.\n", my_rank);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}