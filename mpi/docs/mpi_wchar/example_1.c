#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <wchar.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate a wide character between 2 MPI
 * processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends a wide character to the latter, which prints
 * it.
 **/
int main(int argc, char* argv[])
{
    setlocale(LC_ALL, "");

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
            // Sends the wide character
            wchar_t wideCharacterToSend = L'ᄴ';
            printf("[MPI process %d] I sends wide character: '%C'.\n", my_rank, wideCharacterToSend);
            MPI_Ssend(&wideCharacterToSend, 1, MPI_WCHAR, 1, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receives the wide character
            wchar_t wideCharacterReceived;
            MPI_Recv(&wideCharacterReceived, 1, MPI_WCHAR, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI process %d] I received wide character: '%C'.\n", my_rank, wideCharacterReceived);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
