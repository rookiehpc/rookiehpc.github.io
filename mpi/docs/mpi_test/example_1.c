#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to test for the completion of a non-blocking
 * operation.
 * @details This application is designed to cover both cases:
 * - Issuing an MPI_Test when the operation tested is not complete
 * - Issuing an MPI_Test when the operation tested is complete
 *
 * The application execution flow can be visualised below:
 *
 *               +---------------+-----------+
 *               | Operation not | Operation |
 *               | complete  yet | complete  |
 * +-------------+---------------+-----------+
 * | MPI_Test #1 |        X      |           |
 * | MPI_Test #2 |               |      X    |
 * +-------------+---------------+-----------+
 *
 * This program is meant to be run with 2 processes: a sender and a
 * receiver.
 *
 * (Note to readers: the use of a barrier and a second message message is only
 * to guarantee that the application exposes the execution flow depicted above.)
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

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    if(my_rank == 0)
    {
        // The "master" MPI process sends the message.
        int first_message = 12345;
        int second_message = 67890;
        MPI_Request request;
    
        // Wait for the receiver to issue the MPI_Test meant to fail
        MPI_Barrier(MPI_COMM_WORLD);

        printf("[Process 0] Sends first message (%d).\n", first_message);
        MPI_Isend(&first_message, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &request);
        printf("[Process 0] Sends second message (%d).\n", second_message);
        MPI_Send(&second_message, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
        MPI_Wait(&request, MPI_STATUS_IGNORE);
    }
    else
    {
        // The "slave" MPI process receives the message.
        int first_message;
        int second_message; 
        int ready;
        MPI_Request request;

        MPI_Irecv(&first_message, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &request);

        // The corresponding send has not been issued yet, this MPI_Test will "fail".
        MPI_Test(&request, &ready, MPI_STATUS_IGNORE);
        if(ready)
            printf("[Process 1] MPI_Test #1: message received (%d).\n", first_message);
        else
            printf("[Process 1] MPI_Test #1: message not received yet.\n");

        // Tell the sender that we issued the MPI_Test meant to fail, it can now send the message.
        MPI_Barrier(MPI_COMM_WORLD);

        MPI_Recv(&second_message, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        printf("[Process 1] Second message received (%d), which implies that the first message is received too.\n", second_message);
        MPI_Test(&request, &ready, MPI_STATUS_IGNORE);
        if(ready)
            printf("[Process 1] MPI_Test #2: message received (%d).\n", first_message);
        else
            printf("[Process 1] MPI_Test #2: message not received yet.\n");
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
