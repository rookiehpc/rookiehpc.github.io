#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to accumulate data.
 * @details This application consists of two MPI processes. MPI process 0
 * exposes a window containing an integer initialised to 0. All the other MPI
 * processes add their rank to that value. After the MPI_Accumulate is issued, 
 * each MPI process calls on MPI_Win_fence to synchronise. Finally, MPI process
 * 0 prints the total value.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Create the window
    int window_buffer = 0;
    MPI_Win window;
    MPI_Win_create(&window_buffer, sizeof(int), sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &window);
    if(my_rank == 0)
    {
        printf("[MPI process 0] Value in my window_buffer before MPI_Accumulate: %d.\n", window_buffer);
    }
    MPI_Win_fence(0, window);

    if(my_rank > 0)
    {
        // Push my value into the first integer in MPI process 0 window
        MPI_Accumulate(&my_rank, 1, MPI_INT, 0, 0, 1, MPI_INT, MPI_SUM, window);
        printf("[MPI process %d] I accumulate data %d in MPI process 0 window via MPI_Accumulate.\n", my_rank, my_rank);
    }

    // Wait for the MPI_Accumulate issued to complete before going any further
    MPI_Win_fence(0, window);

    if(my_rank == 0)
    {
        printf("[MPI process 0] Value in my window_buffer after MPI_Accumulate: %d.\n", window_buffer);
    }

    // Destroy the window
    MPI_Win_free(&window);

    MPI_Finalize();

    return EXIT_SUCCESS;
}