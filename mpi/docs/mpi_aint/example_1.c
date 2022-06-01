#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to use the MPI_Aint type and the MPI_AINT MPI_Datatype.
 * @details This application consists of 2 MPI processes. The MPI process 0
 * sends the address takes the address of its local variable to the MPI process
 * 1, which prints it.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Check that only 2 MPI Processes are used
    int comm_size;
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    if(comm_size != 2)
    {
        printf("This application is meant to be run with 2 MPI processes, not %d.\n", comm_size);
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    enum roles {SENDER, RECEIVER};
    switch(my_rank)
    {
        case SENDER:
        {
            // Declare a local variable, get its address and send it to MPI Process 0
            int my_variable;
            MPI_Aint my_variable_address;
            MPI_Get_address(&my_variable, &my_variable_address);
            printf("[MPI Procees %d] The address of my local variable is %p.\n", my_rank, (void*)my_variable_address);
            MPI_Send(&my_variable_address, 1, MPI_AINT, 1, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Get the address of the local variable held on MPI process 1 and print it
            MPI_Aint remote_variable_address;
            MPI_Recv(&remote_variable_address, 1, MPI_AINT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("[MPI Procees %d] The address of the remote variable is %p.\n", my_rank, (void*)remote_variable_address);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
