#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to wait on multiple non-blocking routines until at
 * least one of them completes.
 * @details This program is meant to be run with 4 processes: 1 sender and 3
 * receivers. The sender emits 3 messages using non-blocking sends, one to each
 * receiver. It then uses MPI_Testsome to see which non-blocking routines
 * completed. This application covers multiple cases:
 * - Multiple non-blocking routines completed
 * - Single non-blocking routines completed
 * 
 * The execution flow can be visualised below:
 *
 *                       +-----------+-----------+-----------+
 *                       | Process 1 | Process 2 | Process 3 |
 * +---------------------+-----------+-----------+-----------+
 * | First MPI_Testsome  |     x     |     x     |           |
 * | Second MPI_Testsome |           |           |     x     |
 * +---------------------+-----------+-----------+-----------+
 *
 * (Note to readers: the MPI barriers used in this code are present just to make
 * sure that the application always exposes the execution flow depicted above.)
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the number of processes and check only 4 processes are used
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 4)
    {
        printf("This application is meant to be run with 4 processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    switch(my_rank)
    {
        case 0:
        {
            // The "master" MPI process sends the messages.
            int buffer[3] = {123, 456, 789};
            MPI_Request requests[3];
            int recipient_rank_of_request[3];

            // Send the messages
            for(int i = 0; i < 3; i++)
            {
                printf("[Process %d] Sends %d to process %d.\n", my_rank, buffer[i], i + 1);
                MPI_Issend(&buffer[i], 1, MPI_INT, i + 1, 0, MPI_COMM_WORLD, &requests[i]);
                recipient_rank_of_request[i] = i + 1;
            }

            // Barrier to make sure that the sends 1 and 2 are complete by the first MPI_Testsome
            MPI_Barrier(MPI_COMM_WORLD);

            // Test which of the non-blocking sends to complete
            int index_count;
            int indices[3];
            MPI_Testsome(3, requests, &index_count, indices, MPI_STATUSES_IGNORE);
            for(int i = 0; i < index_count; i++)
            {
                printf("[Process %d] First MPI_Testsome: the non-blocking send to process %d is complete.\n", my_rank, recipient_rank_of_request[indices[i]]);
            }

            // Tell process 3 to do the receive the message
            MPI_Barrier(MPI_COMM_WORLD);

            // Wait for process 3 to tell us the message has been received
            MPI_Barrier(MPI_COMM_WORLD);

            // Test if the other non-blocking send to complete
            MPI_Testsome(3, requests, &index_count, indices, MPI_STATUSES_IGNORE);
            for(int i = 0; i < index_count; i++)
            {
                printf("[Process %d] Second MPI_Testsome: the non-blocking send to process %d is complete.\n", my_rank, recipient_rank_of_request[indices[i]]);
            }
            break;
        }
        case 3:
        {
            // Process 1 and 2 saying they completed their receive
            MPI_Barrier(MPI_COMM_WORLD);

            // Signal for process 3 to receive its message
            MPI_Barrier(MPI_COMM_WORLD);

            // The last MPI process will wait on the barrier before receiving the message.
            int received;
            MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[Process %d] Received value %d.\n", my_rank, received);

            // Tell the master process that the MPI_Recv is complete
            MPI_Barrier(MPI_COMM_WORLD);
            break;
        }
        default:
        {
            // The MPI processes 1 and 2 will receive the message, then they wait on the barrier.
            int received;
            MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[Process %d] Received value %d.\n", my_rank, received);

            // Tell master process that process 1 and 2 have received their message
            MPI_Barrier(MPI_COMM_WORLD);

            // Master process telling process 3 to start receiving
            MPI_Barrier(MPI_COMM_WORLD);

            // Process 3 saying it received its message
            MPI_Barrier(MPI_COMM_WORLD);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
