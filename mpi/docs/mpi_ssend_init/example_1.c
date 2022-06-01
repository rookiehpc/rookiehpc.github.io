#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to synchronously send a message using persistent
 * communications.
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
            int buffer_sent;
            MPI_Request request;
            // Prepare the synchronous send request handle
            MPI_Ssend_init(&buffer_sent, 1, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD, &request);
            for(int i = 0; i < 3; i++)
            {
                buffer_sent = 12345 + i;
                // Launch the synchronous send
                MPI_Start(&request);
                // Wait for the synchronous send to complete
                MPI_Wait(&request, MPI_STATUS_IGNORE);
                printf("MPI process %d sends value %d for message %d.\n", my_rank, buffer_sent, i);
                
            }
            break;
        }
        case RECEIVER:
        {
            int received;
            for(int i = 0; i < 3; i++)
            {
                MPI_Recv(&received, 1, MPI_INT, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                printf("MPI process %d received value %d for message %d.\n", my_rank, received, i);
            }
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
