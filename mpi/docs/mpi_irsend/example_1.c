#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to issue a message as soon as possible in a
 * non-blocking fashion.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver.
 *
 * (Note to readers: the use of an MPI barrier is to ensure that the MPI_Irsend 
 * is issued after the corresponding MPI receive is issued.)
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the number of processes and check only 2 processes are used
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
            MPI_Barrier(MPI_COMM_WORLD);

            int buffer_sent = 12345;
            MPI_Request request;
            printf("MPI process %d sends value %d.\n", my_rank, buffer_sent);
            MPI_Irsend(&buffer_sent, 1, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD, &request);

            // Do something else while the MPI_Irsend progresses
            // <...>

            // Wait for the underlying MPI_Rsend to complete.
            MPI_Wait(&request, MPI_STATUS_IGNORE);
            break;
        }
        case RECEIVER:
        {
            int received;
            MPI_Request request;
            MPI_Irecv(&received, 1, MPI_INT, SENDER, 0, MPI_COMM_WORLD, &request);

            // Tell the other process that the receive is posted, so the ready send can be issued
            MPI_Barrier(MPI_COMM_WORLD);

            // Wait for the underlying MPI_Recv to complete.
            MPI_Wait(&request, MPI_STATUS_IGNORE);
            printf("MPI process %d receives value %d.\n", my_rank, received);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
