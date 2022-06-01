#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use a minloc reduction operation.
 * @details This application consists in every MPI process sending its value
 * along with its rank. The value sent will be used in the min reduction while
 * the rank will be the locator data. This allows for the locator to represent
 * the rank which has the minimum value found. The result of the reduction will
 * be stored on MPI process 2. It can be visualised as follows:
 *
 * +--------------+ +--------------+ +--------------+ +--------------+
 * |   Process 0  | |   Process 1  | |   Process 2  | |   Process 3  |
 * +--------------+ +--------------+ +--------------+ +--------------+
 * | Value:    12 | | Value:    34 | | Value:    56 | | Value:    78 |
 * | Location:  0 | | Location:  1 | | Location:  2 | | Location:  3 |
 * +--------------+ +--------------+ +--------------+ +--------------+
 *            \             |               |             /
 *             \            |               |            /
 *              \           |               |           /
 *               \          |               |          /
 *                +---------+------+--------+---------+
 *                                 |
 *                          +--------------+
 *                          |     MIN      |
 *                          +--------------+
 *                          | Value:    12 |
 *                          | Location:  0 |
 *                          +--------------+
 *                                 |
 *                          +--------------+
 *                          |   Process 2  |
 *                          +--------------+
 *                          | Value:    12 |
 *                          | Location:  0 |
 *                          +--------------+
 * 
 * In order to pass the value and the locator data, an MPI datatype containing
 * both will be created.
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
    int root_rank = 2;

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // The data pair that will be used.
    // [0]: the value (12 / 34 / 56 / 78)
    // [1]: the locator (the MPI rank)
    int data_pair[2];

    // Initialise the value
    switch(my_rank)
    {
        case 0: data_pair[0] = 12; break;
        case 1: data_pair[0] = 34; break;
        case 2: data_pair[0] = 56; break;
        case 3: data_pair[0] = 78; break;
    }

    // Initialise the locator
    data_pair[1] = my_rank;

    // Each MPI process sends its rank to reduction, root MPI process collects the result
    int reduction_result[2];
    MPI_Reduce(data_pair, reduction_result, 1, MPI_2INT, MPI_MINLOC, root_rank, MPI_COMM_WORLD);

    if(my_rank == root_rank)
    {
        printf("The minimum value is %d and is held by MPI process %d.\n", reduction_result[0], reduction_result[1]);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
