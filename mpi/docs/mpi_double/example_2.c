#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate an array of doubles between 2 MPI
 * processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends an array of doubles to the latter, which
 * prints it.
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
            // Send the doubles
            double doublesToSend[2] = { 12.34, 56.78 };
            printf("[MPI process %d] I send doubles: %.2f and %.2f.\n", my_rank, doublesToSend[0], doublesToSend[1]);
            MPI_Ssend(doublesToSend, 2, MPI_DOUBLE, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receive the doubles
            double doublesReceived[2];
            MPI_Recv(doublesReceived, 2, MPI_DOUBLE, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI process %d] I received doubles: %.2f and %.2f.\n", my_rank, doublesReceived[0], doublesReceived[1]);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
