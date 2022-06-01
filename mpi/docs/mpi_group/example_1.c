#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates what is a group.
 * @details This code obtains the group of processes making the default
 * communicator, then queries that group to know how many processes are part of
 * it.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the group from the default communicator
    MPI_Group group;
    MPI_Comm_group(MPI_COMM_WORLD, &group);

    // Get the size of the group
    int size;
    MPI_Group_size(group, &size);

    // Each process prints the number of processes in that group
    printf("We are %d MPI processes in the group of the default communicator MPI_COMM_WORLD.\n", size);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
