#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use a non-blocking reduce scatter block.
 * @details This application is meant to be run with 3 MPI processes. It
 * consists of a sum reduction; every MPI process has three values to send for
 * reduction. The first values from the MPI process will be reduced and stored 
 * on the MPI process 0. The second values will be reduced separately and stored
 * on MPI process 1, similarly with the third values on MPI process 2. It can be
 * visualised as follows:
 *
 *      +-----------+  +-----------+  +-----------+
 *      | Process 0 |  | Process 1 |  | Process 2 |
 *      +-----------+  +-----------+  +-----------+
 *      |   Values  |  |   Values  |  |   Values  |
 *      +---+---+---+  +---+---+---+  +---+---+---+ 
 *      | 0 | 1 | 2 |  | 3 | 4 | 5 |  | 6 | 7 | 8 |
 *      +---+---+---+  +---+---+---+  +---+---+---+
 *        |    \   \     /   |    \    /   /    |
 *        | ____\___\___/____|_____\__/   /     |
 *        |/     \   \       |      \    /      |
 *        |       \___\_____ | ______\__/       |
 *        |            \    \|/       \         |
 *        |             \____|_________\_______ |
 *        |                  |                 \|
 *        |                  |                  |
 *     +--+--+            +--+--+            +-----+
 *     | SUM |            | SUM |            | SUM |
 *     +-----+            +-----+            +-----+
 *     |  9  |            |  12 |            |  15 |
 *     +--+--+            +--+--+            +--+--+
 *        |                  |                  |      
 *  +-----+-----+      +-----+-----+      +-----+-----+
 *  | Process 0 |      | Process 1 |      | Process 2 |
 *  +-----------+      +-----------+      +-----------+
 *  |   Value   |      |   Value   |      |   Value   |
 *  |     9     |      |     12    |      |     15    |
 *  +-----------+      +-----------+      +-----------+
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the size of the communicator
    int size = 0;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 3)
    {
        printf("This application is meant to be run with 3 MPI processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Defines my values
    int values[3] = {3 * my_rank, 3 * my_rank + 1, 3 * my_rank + 2};

    // Each MPI process sends its values and the buffer to receive the corresponding reduction variables
    int reduction_result;
    MPI_Request request;
    MPI_Ireduce_scatter_block(values, &reduction_result, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &request);

    // Do some job while it progresses
    // ...

    // Wait for the MPI_Ireduce_scatter_block to complete
    MPI_Wait(&request, MPI_STATUS_IGNORE);
    printf("[MPI process %d] The sum I received is %d.\n", my_rank, reduction_result);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
