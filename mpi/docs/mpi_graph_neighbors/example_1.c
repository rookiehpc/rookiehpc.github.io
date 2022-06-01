#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <mpi.h>

/**
 * @brief Illustrates how to retrieve the adjacency list of an MPI process.
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
 * After creating the graph, each MPI process retrieves its list of neighbours
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

    // To simplify, make sure we keep our rank in the new communicator
    int reorder = false;

    // Create a communicator given the graph topology.
    MPI_Comm new_communicator;
    MPI_Graph_create(MPI_COMM_WORLD, comm_size, indexes, edges, reorder, &new_communicator);

    // Get my number of neighbours
    int number_of_neighbours_retrieved;
    MPI_Graph_neighbors_count(new_communicator, my_rank, &number_of_neighbours_retrieved);

    // Allocate the array in which store the neighbour ranks, if any
    if(number_of_neighbours_retrieved > 0)
    {
        int* neighbours_retrieved = (int*)malloc(sizeof(int) * number_of_neighbours_retrieved);
        MPI_Graph_neighbors(new_communicator, my_rank, number_of_neighbours_retrieved, neighbours_retrieved);
        printf("[MPI process %d] I have %d neighbours: ", my_rank, number_of_neighbours_retrieved);
        for(int i = 0; i < number_of_neighbours_retrieved; i++)
        {
            printf("%d", neighbours_retrieved[i]);
            if(i < number_of_neighbours_retrieved - 1)
            {
                printf(", ");
            }
        }
        printf("\n");
        free(neighbours_retrieved);
    }
    else
    {
        printf("[MPI process %d] I have no neighbours.\n", my_rank);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}