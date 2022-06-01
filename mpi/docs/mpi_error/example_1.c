#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Display the error code contained in the MPI_Status.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get my rank and do the corresponding job
    enum role_ranks { SENDER, RECEIVER };
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    switch(my_rank)
    {
        case SENDER:
        {
            // The "master" MPI process issues the MPI_Bsend.
            int buffer_sent = 12345;
            printf("MPI process %d sends value %d.\n", my_rank, buffer_sent);
            MPI_Ssend(&buffer_sent, 1, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // The "slave" MPI process receives the message.
            int buffer_received;
            MPI_Status status;
            MPI_Recv(&buffer_received, 1, MPI_INT, SENDER, 0, MPI_COMM_WORLD, &status);
            printf("MPI process %d received value %d, with error code %d.\n", my_rank, buffer_received, status.MPI_ERROR);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
