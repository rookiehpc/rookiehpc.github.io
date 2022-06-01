#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to get data from a target window.
 * @details This application consists of two MPI processes. MPI process 1
 * exposes a window containing an integer. MPI process 0 gets the value in it.
 * After the MPI_Get is issued, synchronisation takes place via MPI_Win_fence
 * and the MPI process 1 prints the value in its window.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Check that only 2 MPI processes are spawn
    int comm_size;
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    if(comm_size != 2)
    {
        printf("This application is meant to be run with 2 MPI processes, not %d.\n", comm_size);
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Create the window
    int window_buffer = 0;
    if(my_rank == 1)
    {
        window_buffer = 12345;
    }
    MPI_Win window;
    MPI_Win_create(&window_buffer, sizeof(int), sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &window);
    MPI_Win_fence(0, window);

    int value_fetched;
    if(my_rank == 0)
    {
        // Fetch the value from the MPI process 1 window
        MPI_Get(&value_fetched, 1, MPI_INT, 1, 0, 1, MPI_INT, window);
    }

    // Wait for the MPI_Get issued to complete before going any further
    MPI_Win_fence(0, window);

    if(my_rank == 0)
    {
        printf("[MPI process 0] Value fetched from MPI process 1 window: %d.\n", value_fetched);
    }

    // Destroy the window
    MPI_Win_free(&window);

    MPI_Finalize();

    return EXIT_SUCCESS;
}