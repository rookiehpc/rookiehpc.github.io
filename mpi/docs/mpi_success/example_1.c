#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Uses the MPI_SUCCESS constant to check the success of any MPI routine.
 **/
int main(int argc, char* argv[])
{
    if(MPI_Init(&argc, &argv) == MPI_SUCCESS)
    {
        printf("The MPI routine MPI_Init succeeded.\n");
    }
    else
    {
        printf("The MPI routine MPI_Init failed.\n");
    }

    int size;
    if(MPI_Comm_size(MPI_COMM_WORLD, &size) == MPI_SUCCESS)
    {
        printf("The MPI routine MPI_Comm_size succeeded.\n");
    }
    else
    {
        printf("The MPI routine MPI_Comm_size failed.\n");
    }

    if(MPI_Finalize() == MPI_SUCCESS)
    {
        printf("The MPI routine MPI_Finalize succeeded.\n");
    }
    else
    {
        printf("The MPI routine MPI_Finalize failed.\n");
    }

    return EXIT_SUCCESS;
}
