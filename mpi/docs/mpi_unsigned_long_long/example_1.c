#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate a unsigned long long int between 2 MPI
 * processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends a unsigned long long int to the latter,
 * which prints it.
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
            // Send the unsigned long long int
            unsigned long long int unsignedLongLongIntToSend = 12345;
            printf("[MPI process %d] I send unsigned long long int: %llu.\n", my_rank, unsignedLongLongIntToSend);
            MPI_Ssend(&unsignedLongLongIntToSend, 1, MPI_UNSIGNED_LONG_LONG, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receive the unsigned long long int
            unsigned long long int unsignedLongLongIntReceived;
            MPI_Recv(&unsignedLongLongIntReceived, 1, MPI_UNSIGNED_LONG_LONG, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI process %d] I received unsigned long long int: %llu.\n", my_rank, unsignedLongLongIntReceived);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
