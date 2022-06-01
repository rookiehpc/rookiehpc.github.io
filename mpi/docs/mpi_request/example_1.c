#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use an MPI_Request to wait for the completion of a
 * non-blocking operation.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver.
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
            int buffer = 12345;
            printf("MPI process %d sends the value %d.\n", my_rank, buffer);
            MPI_Ssend(&buffer, 1, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            int received;
            MPI_Request request;
            MPI_Irecv(&received, 1, MPI_INT, SENDER, 0, MPI_COMM_WORLD, &request);

            // Do some other things while the underlying MPI_Recv progresses.
            printf("MPI process %d issued the MPI_Irecv and obtained an MPI_Request.\n", my_rank);

            // Wait for the MPI_Recv to complete.
            printf("MPI process %d waits on that MPI_Request.\n", my_rank);
            MPI_Wait(&request, MPI_STATUS_IGNORE);
            printf("The MPI_Wait completed, which means the request (pointing to an MPI_Recv) is complete too (value received: %d).\n", received);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
