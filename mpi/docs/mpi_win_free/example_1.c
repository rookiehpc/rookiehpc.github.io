#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to destroy a window.
 * @details This application consists in creating a window and destroying it.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Create the window
    const int ARRAY_SIZE = 2;
    int window_buffer[ARRAY_SIZE];
    MPI_Win window;
    MPI_Win_create(window_buffer, ARRAY_SIZE * sizeof(int), sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &window);
    printf("[MPI process %d] Window created.\n", my_rank);
    
    // Issue RMA communications
    // ...
    // Wait for all RMA communications to complete

    // Destroy the window
    MPI_Win_free(&window);
    printf("[MPI process %d] Window destroyed.\n", my_rank);

    MPI_Finalize();

    return EXIT_SUCCESS;
}