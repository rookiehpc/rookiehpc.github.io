#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate an array of unsigned short ints between
 * 2 MPI processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends an array of unsigned short ints to the
 * latter, which prints it.
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
            // Send the unsigned short ints
            unsigned short int unsignedShortIntsToSend[2] = { 1234, 5678 };
            printf("[MPI process %d] I send unsigned short ints: %d and %d.\n", my_rank, unsignedShortIntsToSend[0], unsignedShortIntsToSend[1]);
            MPI_Ssend(unsignedShortIntsToSend, 2, MPI_UNSIGNED_SHORT, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receive the unsigned short ints
            unsigned short int unsignedShortIntsReceived[2];
            MPI_Recv(unsignedShortIntsReceived, 2, MPI_UNSIGNED_SHORT, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI process %d] I received unsigned short ints: %d and %d.\n", my_rank, unsignedShortIntsReceived[0], unsignedShortIntsReceived[1]);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
