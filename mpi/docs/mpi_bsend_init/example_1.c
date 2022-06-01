#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to send a message in a blocking asynchronous fashion
 * using persistent communications.
 * @details This application is meant to be used with 2 processes; 1 sender and
 * 1 receiver.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the number of processes and check only 2 are used.
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
            // Declare the buffer and attach it
            int buffer_attached_size = MPI_BSEND_OVERHEAD + sizeof(int);
            char* buffer_attached = (char*)malloc(buffer_attached_size);
            MPI_Buffer_attach(buffer_attached, buffer_attached_size);

            // Prepare the MPI_Bsend
            int buffer_sent;
            MPI_Request request;
            MPI_Bsend_init(&buffer_sent, 1, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD, &request);

            for(int i = 0; i < 3; i++)
            {
                buffer_sent = 12345 + i;
                printf("[MPI process %d] I send value %d for message %d.\n", my_rank, buffer_sent, i);
                // Launch the buffered send
                MPI_Start(&request);
                // Wait for the completion of the buffered send
                MPI_Wait(&request, MPI_STATUS_IGNORE);
            }

            // Detach the buffer. It blocks until all messages stored are sent.
            MPI_Buffer_detach(&buffer_attached, &buffer_attached_size);
            free(buffer_attached);
            break;
        }
        case RECEIVER:
        {
            // Receive the message and print it.
            int received;
            for(int i = 0; i < 3; i++)
            {
                MPI_Recv(&received, 1, MPI_INT, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                printf("[MPI process %d] I received value %d for message %d.\n", my_rank, received, i);
            }
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
