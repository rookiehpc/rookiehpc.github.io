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
 * @brief Illustrates how to run a user-defined operation for reduction.
 * @details This application consists of 3 MPI processes that participate to a
 * sum reduction using a user-defined function for the sum. Each MPI process 
 * sends two integers for reduction:
 * 1) its rank
 * 2) its rank plus the communicator size.
 * It can be visualised as follows:
 *
 *         "inputBuffer"     "inputBuffer"     "inputBuffer"
 *              on                on                on
 *         MPI process 0     MPI process 1     MPI process 2     "outputBuffer"                    
 *     ^ +---------------+ +---------------+ +---------------+ +---------------+
 *     | |       0       | |       1       | |       2       | |   0+1+2 = 3   |
 * len | +---------------+ +---------------+ +---------------+ +---------------+
 *     | |       3       | |       4       | |       5       | |   3+4+5 = 12  |
 *     v +---------------+ +---------------+ +---------------+ +---------------+
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

    MPI_Finalize();

    return EXIT_SUCCESS;
}
