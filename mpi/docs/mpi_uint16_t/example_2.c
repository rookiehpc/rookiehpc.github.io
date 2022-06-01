#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate an array of unsigned 2-byte ints between
 * 2 MPI processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends an array of unsigned 2-byte ints to the
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
            // Send the ints
            uint16_t intsToSend[2] = { 1234, 5678 };
            printf("[MPI process %d] I send unsigned ints: %u and %u.\n", my_rank, intsToSend[0], intsToSend[1]);
            MPI_Ssend(intsToSend, 2, MPI_UINT16_T, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receive the ints
            uint16_t intsReceived[2];
            MPI_Recv(intsReceived, 2, MPI_UINT16_T, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI process %d] I received unsigned ints: %u and %u.\n", my_rank, intsReceived[0], intsReceived[1]);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
