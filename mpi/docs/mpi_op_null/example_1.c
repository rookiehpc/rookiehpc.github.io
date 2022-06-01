#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief User-defined version of a sum function for reduction.
 * @param[in] inputBuffer A pointer on the buffer providing the inputs of an
 * MPI process.
 * @param[inout] outputBuffer A pointer on the buffer in which write the
 * reduction results.
 * @param[in] len The number of elements on which the reduction applies. This is
 * not the number of MPI processes in the communicator but the "count" argument
 * passed to the reduction call.
 **/
void my_sum_function(void* inputBuffer, void* outputBuffer, int* len, MPI_Datatype* datatype)
{
    int* input = (int*)inputBuffer;
    int* output = (int*)outputBuffer;

    for(int i = 0; i < *len; i++)
    {
        output[i] += input[i];
    }
}

/**
 * @brief Illustrates that the value of a user-defined operation handle is 
 * MPI_OP_NULL once it has been deallocated by MPI_Op_free.
 * @details This application consists of 3 MPI processes that participate to a
 * user-defined reduction. The operation handle used for that purpose is then
 * deallocated, and its value after deallocatin is checked against MPI_OP_NULL.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the number of processes and check only 4 are used.
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 3)
    {
        printf("This application is meant to be run with 3 processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Determine root's rank
    int root_rank = 0;

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Create the operation handle
    MPI_Op operation;
    MPI_Op_create(&my_sum_function, 1, &operation);

    // Initialise the data to send
    int data[2] = { my_rank, my_rank + size };

    // Each MPI process sends its rank to reduction, root MPI process collects the result
    int reduction_results[2] = { 0, 0 };
    MPI_Reduce(data, reduction_results, 2, MPI_INT, operation, root_rank, MPI_COMM_WORLD);

    if(my_rank == root_rank)
    {
        printf("The sum of first elements of data is %d.\n", reduction_results[0]);
        printf("The sum of second elements of data is %d.\n", reduction_results[1]);
    }

    // Free the operation handle created
    MPI_Op_free(&operation);

    // Check the operation handle is now reset to MPI_OP_NULL
    if(operation == MPI_OP_NULL)
    {
        printf("[MPI Process %d] The operation handle is now reset back to MPI_OP_NULL.\n", my_rank);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
