#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to probe a message in a non-blocking way.
 * @details This application is designed to probe a message at a moment at which
 * the message to receive is guaranteed not to have been arrived yet. The
 * MPI_Iprobe therefore informs that there is no message waiting and returns,
 * while an MPI_Probe would have blocked until the arrival of that message.
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

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    if(my_rank == 0)
    {    
        // Wait for the other process to issue an MPI_Iprobe in vain
        MPI_Barrier(MPI_COMM_WORLD);

        // Send the message
        int buffer = 12345;
        printf("Process %d: sending the message containing %d.\n", my_rank, buffer);
        MPI_Send(&buffer, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
    }
    else
    {
        // The send has not been issued yet, this probe is vain but not blocking.
        int flag;
        MPI_Iprobe(0, 0, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
        if(!flag)
        {
            printf("Process %d: no message arrived yet.\n", my_rank);
        }
        else
        {
            // This branching will not happen
            printf("Process %d: message arrived.\n", my_rank);
        }

        // Inform other MPI process that we issued the MPI_Iprobe meant to be vain
        MPI_Barrier(MPI_COMM_WORLD);

        // Actually receive the message
        int buffer;
        MPI_Recv(&buffer, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        printf("Process %d: message received containing %d.\n", my_rank, buffer);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}