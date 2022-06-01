#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use the MPI_REQUEST_NULL value.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver. The MPI process 0 sends an integer to MPI process 1 using a
 * non-blocking standard send. MPI process 0 then checks that the request is
 * set to MPI_REQUEST_NULL once it has been waited upon.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get the number of processes and check only 2 processes are used
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 2)
    {
        printf("This application is meant to be run with 2 processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank and do the corresponding job
    enum role_ranks { SENDER, RECEIVER };
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    switch(my_rank)
    {
        case SENDER:
        {
            int buffer_sent = 12345;
            MPI_Request request;
            MPI_Isend(&buffer_sent, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &request);
            
            // Do other things while the MPI_Isend completes
            // <...>

            // Let's wait for the MPI_Isend to complete before progressing further and check the request has been set to MPI_REQUEST_NULL.
            MPI_Wait(&request, MPI_STATUS_IGNORE);
            printf("[MPI process 0] The MPI_Wait completed. The request has been set to MPI_REQUEST_NULL: %s.\n", request == MPI_REQUEST_NULL ? "true" : "false");
            break;
        }
        case RECEIVER:
        {
            int received;
            MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
