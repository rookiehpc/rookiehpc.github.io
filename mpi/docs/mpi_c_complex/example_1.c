#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate a complex float between 2 MPI processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends a complex float to the latter, which prints
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
            // Send the complex
            float _Complex complexToSend = I * I;
            printf("[MPI process %d] I send complex: %.1f+%.1fi.\n", my_rank, crealf(complexToSend), cimagf(complexToSend));
            MPI_Ssend(&complexToSend, 1, MPI_C_COMPLEX, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receive the complex
            float _Complex complexReceived;
            MPI_Recv(&complexReceived, 1, MPI_C_COMPLEX, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI process %d] I received complex: %.1f+%.1fi.\n", my_rank, crealf(complexReceived), cimagf(complexReceived));
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
