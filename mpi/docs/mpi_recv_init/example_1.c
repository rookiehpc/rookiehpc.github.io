#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to receive a message using persistent communications.
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
            // The "master" MPI process issues the MPI_Ssend.
            int buffer_sent;
            for(int i = 0; i < 3; i++)
            {
                buffer_sent = 12345 + i;
                printf("MPI process %d sends value %d for message %d.\n", my_rank, buffer_sent, i);
                MPI_Ssend(&buffer_sent, 1, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD);
            }
            break;
        }
        case RECEIVER:
        {
            // The "slave" MPI process receives the message.
            int received;
            MPI_Request request;
            // Fill a request handle describing the arguments to pass for reception
            MPI_Recv_init(&received, 1, MPI_INT, SENDER, 0, MPI_COMM_WORLD, &request);
            for(int i = 0; i < 3; i++)
            {
                // Launch the reception
                MPI_Start(&request);
                // Wait for the reception to complete
                MPI_Wait(&request, MPI_STATUS_IGNORE);
                printf("MPI process %d received value %d for message %d.\n", my_rank, received, i);
            }
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
