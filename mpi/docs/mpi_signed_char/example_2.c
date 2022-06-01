#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate an array of signed chars between 2 MPI
 * processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends an array of signed chars to the latter,
 * which prints it.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Check that 2 MPI processes are used.
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 2)
    {
        printf("This application is meant to be run with 2 MPI processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank and do the corresponding job.
    enum role_ranks { SENDER, RECEIVER };
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    switch(my_rank)
    {
        case SENDER:
        {
            // Sends the array of signed characters
            signed char signedCharsToSend[2] = { 'A', 'B' };
            printf("[MPI process %d] I send signed chars: '%c' and '%c'.\n", my_rank, signedCharsToSend[0], signedCharsToSend[1]);
            MPI_Ssend(signedCharsToSend, 2, MPI_SIGNED_CHAR, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receives the array of signed characters
            signed char signedCharsReceived[2];
            MPI_Recv(signedCharsReceived, 2, MPI_SIGNED_CHAR, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI process %d] I received signed chars: '%c' and '%c'.\n", my_rank, signedCharsReceived[0], signedCharsReceived[1]);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
