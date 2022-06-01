#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates
 * @details MPI processes split into two groups depending on whether their rank
 * is even.
 *
 * +----------------+---+---+---+---+
 * | MPI processes  | 0 | 1 | 2 | 3 |
 * +----------------+---+---+---+---+
 * | MPI_COMM_WORLD | X | X | X | X |
 * | Subgroup A     | X |   | X |   |
 * | Subgroup B     |   | X |   | X |
 * +----------------+---+---+---+---+
 *
 * In subcommunicator A, MPI processes are assigned ranks in the same order as
 * their rank in the global communicator.
 * In subcommunicator B, MPI processes are assigned ranks in the opposite order
 * as their rank in the global communicator.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Check that 4 MPI processes are used
    int comm_size;
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    if(comm_size != 4)
    {
        printf("This application is meant to be run with 4 MPI processes, not %d.\n", comm_size);
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank in the global communicator
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Determine the colour and key based on whether my rank is even.
    char subcommunicator;
    int colour;
    int key;
    if(my_rank % 2 == 0)
    {
        subcommunicator = 'A';
        colour = 0;
        key = my_rank;
    }
    else
    {
        subcommunicator = 'B';
        colour = 1;
        key = comm_size - my_rank;
    }

    // Split de global communicator
    MPI_Comm new_comm;
    MPI_Comm_split(MPI_COMM_WORLD, colour, key, &new_comm);

    // Get my rank in the new communicator
    int my_new_comm_rank;
    MPI_Comm_rank(new_comm, &my_new_comm_rank);

    // Print my new rank and new communicator
    printf("[MPI process %d] I am now MPI process %d in subcommunicator %c.\n", my_rank, my_new_comm_rank, subcommunicator);

    MPI_Finalize();

    return EXIT_SUCCESS;
}