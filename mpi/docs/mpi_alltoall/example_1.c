#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use an all to all.
 * @details This application is meant to be run with 3 MPI processes. Every MPI
 * process begins with a buffer containing 3 integers, one for each process
 * including themselves. They also have a buffer in which receive the integer
 * that has been sent by each other process for them. It can be visualised as
 * follows:
 *
 * +-----------------------+ +-----------------------+ +-----------------------+
 * |       Process 0       | |       Process 1       | |       Process 2       |
 * +-------+-------+-------+ +-------+-------+-------+ +-------+-------+-------+
 * | Value | Value | Value | | Value | Value | Value | | Value | Value | Value |
 * |   0   |  100  |  200  | |  300  |  400  |  500  | |  600  |  700  |  800  |
 * +-------+-------+-------+ +-------+-------+-------+ +-------+-------+-------+
 *     |       |       |_________|_______|_______|_________|___    |       |
 *     |       |    _____________|_______|_______|_________|   |   |       |
 *     |       |___|_____________|_      |      _|_____________|___|       |
 *     |      _____|_____________| |     |     | |_____________|_____      |
 *     |     |     |               |     |     |               |     |     |
 *  +-----+-----+-----+         +-----+-----+-----+         +-----+-----+-----+
 *  |  0  | 300 | 600 |         | 100 | 400 | 700 |         | 200 | 500 | 800 |
 *  +-----+-----+-----+         +-----+-----+-----+         +-----+-----+-----+
 *  |    Process 0    |         |    Process 1    |         |    Process 2    |
 *  +-----------------+         +-----------------+         +-----------------+
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get number of processes and check that 3 processes are used
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 3)
    {
        printf("This application is meant to be run with 3 MPI processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Define my value
    int my_values[3];
    for(int i = 0; i < 3; i++)
    {
        my_values[i] = my_rank * 300 + i * 100;
    }
    printf("Process %d, my values = %d, %d, %d.\n", my_rank, my_values[0], my_values[1], my_values[2]);

    int buffer_recv[3];
    MPI_Alltoall(&my_values, 1, MPI_INT, buffer_recv, 1, MPI_INT, MPI_COMM_WORLD);
    printf("Values collected on process %d: %d, %d, %d.\n", my_rank, buffer_recv[0], buffer_recv[1], buffer_recv[2]);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
