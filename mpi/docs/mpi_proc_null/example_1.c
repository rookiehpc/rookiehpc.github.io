#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to handle boundaries using MPI_PROC_NULL.
 * @details This applications consists of a chain communication, where each
 * process sends its value to its right neighbour (the MPI process with a rank
 * higher by 1), and receives a value from its left neighbour (the MPI process
 * with a rank lower by 1).
 *
 * This can be visualised as follows, assuming 3 MPI processes:
 *
 * +---------------+---------------+---------------+
 * | MPI process 0 | MPI process 1 | MPI process 2 |
 * +---------------+---------------+---------------+
 * |   Value:   0  |   Value: 100  |   Value: 200  |
 * +---------------+---------------+---------------+
 *
 * Each MPI process is going to communicate with a "left" and "right" neighbour,
 * however in a not-cyclic configuration, the first and last MPI processes will
 * "miss" one neighbour, which will be replaced with MPI_PROC_NULL, as follows:
 *
 * MPI_PROC_NULL <-> MPI process 0 <-> MPI process 1 <-> MPI process 2 <-> MPI_PROC_NULL
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Find my rank and the communicator size
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    int comm_size;
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);

    // Determine the ranks of our neighbours, or use MPI_PROC_NULL
    int recipient_rank = (my_rank == comm_size - 1) ? MPI_PROC_NULL : my_rank + 1;
    int sender_rank = (my_rank == 0) ? MPI_PROC_NULL : my_rank - 1;

    // Now, all MPI processes can safely issue sends and receive
    // No boundary-related bugs will arise since MPI_PROC_NULL is used in these cases

    // Send my value to my right neighbour (or issue a dummy send via MPI_PROC_NULL if I don't have one)
    int value_to_send = my_rank * 100;
    MPI_Request request;
    MPI_Isend(&value_to_send, 1, MPI_INT, recipient_rank, 0, MPI_COMM_WORLD, &request);
    printf("[MPI Process %d] Sent value %d to MPI process %d\n", my_rank, value_to_send, recipient_rank);

    // Receive the value from my left neighbour (or issue a dummy receive via MPI_PROC_NULL if I don't have one)
    int value_received = -1;
    MPI_Recv(&value_received, 1, MPI_INT, sender_rank, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    printf("[MPI process %d] Value received from MPI Process %d: %d\n", my_rank, sender_rank, value_received);

    // Wait for the completion of the MPI_Isend
    MPI_Wait(&request, MPI_STATUS_IGNORE);

    MPI_Finalize();

    return EXIT_SUCCESS;
}