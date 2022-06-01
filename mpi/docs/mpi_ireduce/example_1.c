#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use a non-blocking reduce.
 * @details This application consists of a sum reduction; every MPI process
 * sends its rank for reduction before the sum of these ranks is stored in the
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
 *                        |  SUM  |
 *                        +---+---+
 *                            |
 *                        +---+---+
 *                        |   6   |
 *                      +-+-------+-+
 *                      | Process 0 |
 *                      +-----------+
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Determine root's rank
    int root_rank = 0;

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Each MPI process sends its rank to reduction, root MPI process collects the result
    int reduction_result = 0;
    MPI_Request request;
    MPI_Ireduce(&my_rank, &reduction_result, 1, MPI_INT, MPI_SUM, root_rank, MPI_COMM_WORLD, &request);

    // Do some other job
    printf("Process %d issued the MPI_Ireduce and has moved on, printing this message.\n", my_rank);

    // Wait for the MPI_Ireduce to complete
    MPI_Wait(&request, MPI_STATUS_IGNORE);

    if(my_rank == root_rank)
    {
        printf("The sum of all ranks is %d.\n", reduction_result);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
