#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use a min reduction operation.
 * @details This application consists of a min reduction; every MPI process
 * sends its rank for reduction before the min of these ranks is stored in the
 * root MPI process. It can be visualised as follows, with MPI process 0 as
 * root:
 *
 * +-----------+ +-----------+ +-----------+ +-----------+
 * | Process 0 | | Process 1 | | Process 2 | | Process 3 |
 * +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+
 *   | Value |     | Value |     | Value |     | Value |
 *   |   0   |     |   1   |     |   2   |     |   3   |
 *   +-------+     +-------+     +-------+     +-------+
 *            \         |           |         /
 *             \        |           |        /
 *              \       |           |       /
 *               \      |           |      /
 *                +-----+-----+-----+-----+
 *                            |
 *                        +---+---+
 *                        |  MIN  |
 *                        +---+---+
 *                            |
 *                        +---+---+
 *                        |   0   |
 *                      +-+-------+-+
 *                      | Process 0 |
 *                      +-----------+
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the number of processes and check only 4 are used.
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 4)
    {
        printf("This application is meant to be run with 4 processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Determine root's rank
    int root_rank = 0;

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Each MPI process sends its rank to reduction, root MPI process collects the result
    int reduction_result = 0;
    MPI_Reduce(&my_rank, &reduction_result, 1, MPI_INT, MPI_MIN, root_rank, MPI_COMM_WORLD);

    if(my_rank == root_rank)
    {
        printf("The min of all ranks is %d.\n", reduction_result);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
