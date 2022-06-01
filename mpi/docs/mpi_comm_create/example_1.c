#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to create a communicator.
 * @details This code extracts the group of processes in the default
 * communicator, then it keeps only the first 2 processes and creates a group
 * containing only these two. Finally, it creates a communicator based on that
 * group of 2, and proceeds to two broadcasts: one in the global communicator,
 * one in the communicator just created.
 * This application is meant to be run with at least 3 processes.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Check that the application is run with at least 3 processes.
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size < 3)
    {
        printf("Please run this application with at least 3 processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get the group or processes of the default communicator
    MPI_Group world_group;
    MPI_Comm_group(MPI_COMM_WORLD, &world_group);

    // Keep only the processes 0 and 1 in the new group.
    int ranks[2] = {0, 1};
    MPI_Group new_group;
    MPI_Group_incl(world_group, 2, ranks, &new_group);

    // Create the new communicator from that group of processes.
    MPI_Comm new_communicator;
    MPI_Comm_create(MPI_COMM_WORLD, new_group, &new_communicator);

    // Do a broadcast between all processes
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    int value;
    MPI_Bcast(&value, 1, MPI_INT, 0, MPI_COMM_WORLD);
    printf("Process %d took part to the global communicator broadcast.\n", my_rank);

    // Let's wait all processes before proceeding to the second phase.
    MPI_Barrier(MPI_COMM_WORLD);

    // Do a broadcast only between the processes of the new communicator.
    if(new_communicator == MPI_COMM_NULL)
    {
        // I am not part of the new communicator, I can't participate to that broadcast.
        printf("Process %d did not take part to the new communicator broadcast.\n", my_rank);
    }
    else
    {
        // I am part of the new communicator, I can participate to that broadcast.
        MPI_Bcast(&value, 1, MPI_INT, 0, new_communicator);
        printf("Process %d took part to the new communicator broadcast.\n", my_rank);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
