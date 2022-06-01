#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use the variable version of a gather.
 * @details Every MPI process begins with a value, the MPI process 0 will gather
 * all these values and print them. The example is designed to cover all cases:
 * - Different displacements
 * - Different receive counts
 * It can be visualised as follows:
 * This application is meant to be run with 3 processes.
 *
 * +-----------+ +-----------+ +-------------------+ 
 * | Process 0 | | Process 1 | |     Process 2     |
 * +-+-------+-+ +-+-------+-+ +-+-------+-------+-+
 *   | Value |     | Value |     | Value | Value |
 *   |  100  |     |  101  |     |  102  |  103  |
 *   +-------+     +-------+     +-------+-------+
 *      |                |            |     |
 *      |                |            |     |
 *      |                |            |     |
 *      |                |            |     |
 *      |                |            |     |
 *      |                |            |     |
 *   +-----+-----+-----+-----+-----+-----+-----+
 *   | 100 |  0  |  0  | 101 |  0  | 102 | 103 |
 *   +-----+-----+-----+-----+-----+-----+-----+
 *   |                Process 0                |
 *   +-----------------------+-----+-----+-----+
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get number of processes and check only 3 processes are used
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 3)
    {
        printf("This application is meant to be run with 3 processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Determine root's process rank
    int root_rank = 0;

    switch(my_rank)
    {
        case 0:
        {
            // Define my value
            int my_value = 100;

            // Define the receive counts
            int counts[3] = {1, 1, 2};

            // Define the displacements
            int displacements[3] = {0, 3, 5};

            int* buffer = (int*)calloc(7, sizeof(int));
            printf("Process %d, my value = %d.\n", my_rank, my_value);
            MPI_Gatherv(&my_value, 1, MPI_INT, buffer, counts, displacements, MPI_INT, root_rank, MPI_COMM_WORLD);
            printf("Values gathered in the buffer on process %d:", my_rank);
            for(int i = 0; i < 7; i++)
            {
                printf(" %d", buffer[i]);
            }
            printf("\n");
            free(buffer);
            break;
        }
        case 1:
        {
            // Define my value
            int my_value = 101;

            printf("Process %d, my value = %d.\n", my_rank, my_value);
            MPI_Gatherv(&my_value, 1, MPI_INT, NULL, NULL, NULL, MPI_INT, root_rank, MPI_COMM_WORLD);
            break;
        }
        case 2:
        {
            // Define my values
            int my_values[2] = {102, 103};

            printf("Process %d, my values = %d %d.\n", my_rank, my_values[0], my_values[1]);
            MPI_Gatherv(my_values, 2, MPI_INT, NULL, NULL, NULL, MPI_INT, root_rank, MPI_COMM_WORLD);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
