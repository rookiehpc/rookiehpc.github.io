#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to detach a memory region from a window.
 * @details This application consits of 2 MPI processes. They create a window
 * dynamically, then MPI process 0 attaches a region to its window and send its
 * address to MPI process 1. Finally, MPI process 1 uses that address as part of
 * an MPI_Put to write data into MPI process 0 window, which prints its value at
 * the end.
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
    MPI_Win window;
    MPI_Win_create_dynamic(MPI_INFO_NULL, MPI_COMM_WORLD, &window);
    MPI_Win_fence(0, window);
    printf("[MPI process %d] Window created dynamically.\n", my_rank);

    int window_buffer;
    if(my_rank == 0)
    {
        // Allocate and attach the memory region to the window on that target MPI process
        MPI_Win_attach(window, &window_buffer, sizeof(int));
        printf("[MPI Process 0] Memory region attached.\n");

        // Get the address of that window and send it to MPI process 1
        MPI_Aint window_buffer_address;
        MPI_Get_address(&window_buffer, &window_buffer_address);
        MPI_Send(&window_buffer_address, 1, MPI_AINT, 1, 0, MPI_COMM_WORLD);
        printf("[MPI process 0] I send the local address of my memory region to MPI process 1.\n");
    }
    else
    {
        // Get the local address of the memory region attached to that window
        MPI_Aint remote_window_buffer_address;
        MPI_Recv(&remote_window_buffer_address, 1, MPI_AINT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        printf("[MPI process 1] Local address of the memory region attached to the window on MPI process 1 received. I can now use that in MPI_Put.\n");

        // Put the data into into that window
        int value = 12345;
        MPI_Put(&value, 1, MPI_INT, 0, remote_window_buffer_address, 1, MPI_INT, window);
        printf("[MPI Process 1] I put value %d in MPI Process 0 window.\n", value);
    }

    // Destroy the window
    MPI_Win_fence(0, window);
    if(my_rank == 0)
    {
        printf("[MPI process 0] Value in my window: %d.\n", window_buffer);
        MPI_Win_detach(window, &window_buffer);
        printf("[MPI process 0] Memory region detached.\n");
    }
    MPI_Win_free(&window);
    printf("[MPI process 0] Window destroyed.\n");

    MPI_Finalize();

    return EXIT_SUCCESS;
}