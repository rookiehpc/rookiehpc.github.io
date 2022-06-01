#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate a long long int between 2 MPI processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends a long long int to the latter, which prints
 * it.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Check that 2 MPI processes are used.
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 2)
    {
        printf("This application is meant to be run with 2 MPI processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank and do the corresponding job.
    enum role_ranks { SENDER, RECEIVER };
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    switch(my_rank)
    {
        case SENDER:
        {
            // Send the long long int
            long long int longLongIntToSend = 12345;
            printf("[MPI process %d] I send long long int: %lld.\n", my_rank, longLongIntToSend);
            MPI_Ssend(&longLongIntToSend, 1, MPI_LONG_LONG_INT, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receive the long long int
            long long int longLongIntReceived;
            MPI_Recv(&longLongIntReceived, 1, MPI_LONG_LONG_INT, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI process %d] I received long long int: %lld.\n", my_rank, longLongIntReceived);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
