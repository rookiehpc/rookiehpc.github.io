#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to receive a message without restricting the rank of
 * the sender.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. It consists in the sender process sending a message to the 
 * receiver process, which will receive it without restricting the sender rank
 * during the reception operation. The receiver processes then concludes by
 * printing the rank of the message sender obtained via the MPI_Status
 * collected.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Check that only 2 MPI processes are used.
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 2)
    {
        printf("This application is meant to be run with 2 MPI processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank and do the corresponding job
    enum role_ranks { SENDER, RECEIVER };
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    switch(my_rank)
    {
        case SENDER:
        {
            // Sends the message.
            int buffer_sent = 12345;
            printf("[MPI process %d] I send value %d.\n", my_rank, buffer_sent);
            MPI_Ssend(&buffer_sent, 1, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receives the message.
            int buffer_received;
            MPI_Status status;
            MPI_Recv(&buffer_received, 1, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);
            printf("[MPI process %d] I received value %d, from rank %d.\n", my_rank, buffer_received, status.MPI_SOURCE);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
