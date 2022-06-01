#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate an array of unsigned long ints between 2
 * MPI processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends an array of unsigned long ints to the
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
            // Send the unsigned long ints
            unsigned long int unsignedLongIntsToSend[2] = { 12345, 67890 };
            printf("[MPI process %d] I send unsigned long ints: %lu and %lu.\n", my_rank, unsignedLongIntsToSend[0], unsignedLongIntsToSend[1]);
            MPI_Ssend(unsignedLongIntsToSend, 2, MPI_UNSIGNED_LONG, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receive the unsigned long ints
            unsigned long int unsignedLongIntsReceived[2];
            MPI_Recv(unsignedLongIntsReceived, 2, MPI_UNSIGNED_LONG, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI process %d] I received unsigned long ints: %lu and %lu.\n", my_rank, unsignedLongIntsReceived[0], unsignedLongIntsReceived[1]);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
