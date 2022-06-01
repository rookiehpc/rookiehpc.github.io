#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Use a variable to tell what MPI_Datatype to use.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    MPI_Datatype msg_type = MPI_INT;
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    if(my_rank == 0)
    {
        int msg = 12345;
        printf("MPI process %d sends value %d.\n", my_rank, msg);
        MPI_Ssend(&msg, 1, msg_type, 1, 0, MPI_COMM_WORLD);
    }
    else
    {
        int msg;
        MPI_Recv(&msg, 1, msg_type, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        printf("MPI process %d received value %d.\n", my_rank, msg);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
