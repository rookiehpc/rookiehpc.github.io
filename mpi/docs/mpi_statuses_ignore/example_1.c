#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use MPI_STATUSES_IGNORE.
 * @details This program is meant to be run with 3 processes: a sender and two
 * receivers.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the number of processes and check only 3 processes are used
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

    if(my_rank == 0)
    {
        // The "master" MPI process sends the message.
        int buffer[2] = {12345, 67890};
        MPI_Request requests[2];
        printf("MPI process %d sends the values %d & %d.\n", my_rank, buffer[0], buffer[1]);
        MPI_Isend(&buffer[0], 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &requests[0]);
        MPI_Isend(&buffer[1], 1, MPI_INT, 2, 0, MPI_COMM_WORLD, &requests[1]);

        // Wait for both routines to complete, but tell MPI we won't need the statuses.
        MPI_Waitall(2, requests, MPI_STATUSES_IGNORE);
        printf("Process %d: both messages have been sent.\n", my_rank);
    }
    else
    {
        // The "slave" MPI processes receive the message.
        int received;
        MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        printf("Process %d received value %d.\n", my_rank, received);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
