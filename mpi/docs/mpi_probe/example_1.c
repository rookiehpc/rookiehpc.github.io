#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to probe a message.
 * @details This application is designed to receive a message with an unknown
 * length. The receiver issues a probe to retrieve information about the
 * incoming message to allocate a buffer with enough space before actually
 * receiving the message.
 * This application is meant to be run with 2 processes: a sender and a
 * receiver.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Size of the default communicator
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 2)
    {
        printf("This application is meant to be run with 2 processes.\n");
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
            int buffer[3] = {123, 456, 789};
            printf("Process %d: sending a message containing 3 ints (%d, %d, %d), but the receiver is not aware of the length.\n", my_rank, buffer[0], buffer[1], buffer[2]);
            MPI_Send(buffer, 3, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Retrieve information about the incoming message
            MPI_Status status;
            MPI_Probe(0, 0, MPI_COMM_WORLD, &status);
            printf("Process %d: obtained message status by probing it.\n", my_rank);

            // Get the number of integers in the message probed
            int count;
            MPI_Get_count(&status, MPI_INT, &count);

            // Allocate the buffer now that we know how many elements there are
            int* buffer = (int*)malloc(sizeof(int) * count);

            // Finally receive the message
            MPI_Recv(buffer, count, MPI_INT, SENDER, 0, MPI_COMM_WORLD, &status);
            printf("Process %d: received message with all %d ints:", my_rank, count);
            for(int i = 0; i < count; i++)
            {
                printf(" %d", buffer[i]);
            }
            printf(".\n");
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}