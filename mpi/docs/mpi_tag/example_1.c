#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Display the tag contained in the MPI_Status.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the size of the communicator
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 2)
    {
        printf("This application is meant to be run with 2 MPI processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    if(my_rank == 0)
    {
        // The "master" MPI process issues the MPI_Bsend.
        int buffer_sent = 12345;
        int tag = 67890;
        printf("MPI process %d sends value %d, with tag %d.\n", my_rank, buffer_sent, tag);
        MPI_Ssend(&buffer_sent, 1, MPI_INT, 1, tag, MPI_COMM_WORLD);
    }
    else
    {
        // The "slave" MPI process receives the message.
        int buffer_received;
        MPI_Status status;
        MPI_Recv(&buffer_received, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
        printf("MPI process %d received value %d, with tag %d.\n", my_rank, buffer_received, status.MPI_TAG);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
