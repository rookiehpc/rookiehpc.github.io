#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to launch the communications represented with an array
 * request handles.
 * @details This program is meant to be run with 3 processes: 1 sender and 2
 * receivers. The sender prepares 2 MPI_Send with MPI_Send_init, then launches
 * both with MPI_Startall before waiting for both with MPI_Waitall. Receivers
 * only issue a common MPI_Recv.
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

    // Get my rank and do the corresponding job
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    if(my_rank == 0)
    {
        int buffers_sent[2] = { 12345, 67890 };
        MPI_Request requests[2];
        // Prepare the send request handle
        MPI_Send_init(&buffers_sent[0], 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &requests[0]);
        MPI_Send_init(&buffers_sent[1], 1, MPI_INT, 2, 0, MPI_COMM_WORLD, &requests[1]);
        printf("MPI process %d sends value %d to process 1.\n", my_rank, buffers_sent[0]);
        printf("MPI process %d sends value %d to process 2.\n", my_rank, buffers_sent[1]);
        // Launch the sends
        MPI_Startall(2, requests);
        // Wait for the send to complete
        MPI_Waitall(2, requests, MPI_STATUSES_IGNORE);
    }
    else
    {
        int received;
        MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        printf("MPI process %d received value %d.\n", my_rank, received);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
