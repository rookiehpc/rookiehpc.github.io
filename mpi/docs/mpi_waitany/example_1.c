#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to wait on multiple non-blocking routines until one of
 * them completes.
 * @details This program is meant to be run with 3 processes: a sender and two
 * receivers. The sender emits 2 messages in a non-blocking fashion. It then
 * waits on the corresponding request handlers to see which operation finishes
 * first. It then repeats the process to wait for the other operation to finish.
 * This application covers two cases:
 * - All request handlers passed are active
 * - Certain request handlers passed have already completed
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
        // The "master" MPI process sends the messages.
        int buffer[2] = {12345, 67890};
        MPI_Request requests[2];
        int recipient_rank_of_request[2];

        // Send first message to process 1
        printf("[Process %d] Sends %d to process 1.\n", my_rank, buffer[0]);
        MPI_Isend(&buffer[0], 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &requests[0]);
        recipient_rank_of_request[0] = 1;

        // Send second message to process 2
        printf("[Process %d] Sends %d to process 2.\n", my_rank, buffer[1]);
        MPI_Isend(&buffer[1], 1, MPI_INT, 2, 0, MPI_COMM_WORLD, &requests[1]);
        recipient_rank_of_request[1] = 2;

        // Wait for one of non-blocking sends to complete
        int index;
        MPI_Waitany(2, requests, &index, MPI_STATUS_IGNORE);
        printf("[Process %d] The non-blocking send to process %d is complete.\n", my_rank, recipient_rank_of_request[index]);

        // Wait for the other non-blocking send to complete
        MPI_Waitany(2, requests, &index, MPI_STATUS_IGNORE);
        printf("[Process %d] The non-blocking send to process %d is complete too.\n", my_rank, recipient_rank_of_request[index]);
    }
    else
    {
        // The "slave" MPI processes receive the message.
        int received;
        MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        printf("[Process %d] Received value %d.\n", my_rank, received);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
