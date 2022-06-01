#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate a unsigned character between 2 MPI
 * processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends a unsigned character to the latter, which
 * prints it.
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
            // Send the unsigned character.
            unsigned char unsignedCharacterToSend = 'A';
            printf("[MPI process %d] I send unsigned character: '%c'.\n", my_rank, unsignedCharacterToSend);
            MPI_Ssend(&unsignedCharacterToSend, 1, MPI_UNSIGNED_CHAR, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receive the unsigned character.
            unsigned char unsignedCharacterReceived;
            MPI_Recv(&unsignedCharacterReceived, 1, MPI_UNSIGNED_CHAR, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI process %d] I received unsigned character: '%c'.\n", my_rank, unsignedCharacterReceived);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
