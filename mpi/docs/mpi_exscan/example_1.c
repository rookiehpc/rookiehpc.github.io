#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to use an MPI_Exscan.
 * @details This program uses MPI_Exscan to compute a progressive sum of ranks. It
 * can be visualised as follows:
 *
 * +---------------+   +---------------+   +---------------+   +---------------+
 * | MPI process 0 |   | MPI process 1 |   | MPI process 2 |   | MPI process 3 |
 * +---------------+   +---------------+   +---------------+   +---------------+
 * |       0       |   |       1       |   |       2       |   |       3       |
 * +-------+-------+   +-------+-------+   +-------+-------+   +-------+-------+
 *         |                   |                   |                    
 *         |                +--+--+                |                    
 *         +----------------| SUM |                |                    
 *         |                +--+--+                |                    
 *         |                   |                +--+--+                 
 *         |                   +----------------| SUM |                 
 *         |                   |                +--+--+                 
 *         |                   |                   |                    
 *         |                   |                   |                    
 *          \                   \                   \                      
 *           \                   \                   \                  
 *            \                   \                   \                 
 *             \_______________    \_______________    \_______________ 
 *                             |                   |                   |
 *                             |                   |                   |
 * +-------+-------+   +-------+-------+   +-------+-------+   +-------+-------+
 * |   undefined   |   |       0       |   |       1       |   |       3       |
 * +---------------+   +---------------+   +---------------+   +---------------+
 * | MPI process 0 |   | MPI process 1 |   | MPI process 2 |   | MPI process 3 |
 * +---------------+   +---------------+   +---------------+   +---------------+
 *                                       
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Get the sum of all ranks up to the one before mine and print it
    int total;
    MPI_Exscan(&my_rank, &total, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    // The result on MPI process 0 is undefined, do not print it
    if(my_rank == 0)
    {
        printf("[MPI process 0] Total = undefined.\n");
    }
    else
    {
        printf("[MPI process %d] Total = %d.\n", my_rank, total);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
