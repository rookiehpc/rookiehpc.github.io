#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief For each process in the group of the default communicator
 * MPI_COMM_WORLD, show their rank.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the group of processes from the default communicator
    MPI_Group group;
    MPI_Comm_group(MPI_COMM_WORLD, &group);

    // Get my rank in that group.
    int my_rank;
    MPI_Group_rank(group, &my_rank);

    // Print my rank in that group.
    printf("I am MPI process %d in the group.\n", my_rank);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
