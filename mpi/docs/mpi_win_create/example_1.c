#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to create a window.
 * @details This application consists of two MPI processes. MPI process 1
 * exposes a window containing 2 integers. The first one is initialised to 0 and
 * will be overwritten by MPI process 0 via MPI_Put to become 12345. The second
 * will be initialised to 67890 and will be read by MPI process 0 via MPI_Get.
 * After these two commands are issued, synchronisation takes place via
 * MPI_Win_fence and each MPI process prints the value that came from the other
 * peer.
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
    const int ARRAY_SIZE = 2;
    int window_buffer[ARRAY_SIZE];
    MPI_Win window;
    if(my_rank == 1)
    {
        window_buffer[1] = 67890;
    }
    MPI_Win_create(window_buffer, ARRAY_SIZE * sizeof(int), sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &window);
    MPI_Win_fence(0, window);

    int remote_value;
    if(my_rank == 0)
    {
        // Fetch the second integer in MPI process 1 window
        MPI_Get(&remote_value, 1, MPI_INT, 1, 1, 1, MPI_INT, window);

        // Push my value into the first integer in MPI process 1 window
        int my_value = 12345;
        MPI_Put(&my_value, 1, MPI_INT, 1, 0, 1, MPI_INT, window);
    }

    // Wait for the MPI_Get and MPI_Put issued to complete before going any further
    MPI_Win_fence(0, window);

    if(my_rank == 0)
    {
        printf("[MPI process 0] Value fetched from MPI process 1 window_buffer[1]: %d.\n", remote_value);
    }
    else
    {
        printf("[MPI process 1] Value put in my window_buffer[0] by MPI process 0: %d.\n", window_buffer[0]);
    }

    // Destroy the window
    MPI_Win_free(&window);

    MPI_Finalize();

    return EXIT_SUCCESS;
}