#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use MPI_Neighbor_allgather collective operator.
 * @details An application where each MPI process creates a MPI_Graph topology and sends its neighbor a single element of type MPI_INT,
 * with the value being the rank of the MPI process in the graph communicator.
 * The rank is sent to every neighbour using MPI_Neighbor_allgather collective operator.
 * 
 * 
 * 
 * The following is a undirected graph.
 *  0 --- 2
 *  | +
 *  |   +
 *  1 --- 3
 *
 * +---------+----------------+---------+------------------+
 * |  Node   |   Neighbours   | Degree  | Cumulated degree |
 * +---------+----------------+---------+------------------+
 * |    0    | 1, 2, 3        |    3    |      3           |
 * |    1    | 0, 3           |    2    |      5           |
 * |    2    | 0              |    1    |      6           |
 * |    3    | 0, 1           |    2    |      8           |
 * +---------+----------------+----------------------------+
 *
 *
 *  Data transfers
 *  +--------+------+---------+
 *  |  From  |  To  |  Value  |
 *  +--------+------+---------+
 *  |  0     |  1   |  0      |
 *  |  0     |  2   |  0      |
 *  |  0     |  3   |  0      |
 *  |  1     |  0   |  1      |
 *  |  1     |  3   |  1      |
 *  |  2     |  0   |  2      |
 *  |  3     |  0   |  3      |
 *  |  3     |  1   |  3      |
 *  +--------+------+---------+
 **/

int main(int argc, char* argv[])
{
    // MPI variables init
    MPI_Init(NULL, NULL);
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    int count_neighbors;

    const int count_nodes = 4;                         // Numbers of nodes
    const int count_edges = 8;                         // Number of edges
    int indexes[count_nodes] = {3, 5, 6, 8};           // Degree sum
    int edges[count_edges] = {1, 2, 3, 0, 3, 0, 0, 1}; // Flattened edges

    MPI_Comm graph;
    MPI_Graph_create(MPI_COMM_WORLD, count_nodes, indexes, edges, 0, &graph);
    MPI_Comm_rank(graph, &rank); // Sync rank with new communicator

    MPI_Graph_neighbors_count(graph, rank, &count_neighbors); // Get how many neighbors this node has.

    int count_elements = count_neighbors * 1;               // neighbors * elements per neighbor
    size_t buffer_size = count_elements * sizeof(int); // number of elements elements * size of datatype
    int* buffer = (int*)malloc(buffer_size);     // create buffer

    MPI_Neighbor_allgather(&rank, 1, MPI_INT, buffer, 1, MPI_INT, graph); // send rank and gather to buffer

    // Print gathered elements
    for (int i = 0; i < count_elements; i++)
    {
        printf("[MPI process %d] buffer[%d] = %d.\n", rank, i, buffer[i]);
    }

    MPI_Comm_free(&graph);
    free(buffer);

    MPI_Finalize();
    return EXIT_SUCCESS;
}