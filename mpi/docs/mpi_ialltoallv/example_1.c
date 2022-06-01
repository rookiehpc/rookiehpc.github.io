#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use a non-blocking variable all to all.
 * @details This application is meant to be run with 3 MPI processes. Each
 * process has an arbitrary number of elements to send and receive, at different
 * positions. To demonstrate the great flexibility of the MPI_Alltoallv routine,
 * the data exchange designed is rather irregular, so it is extra detailed in
 * this description.
 * 
 * It can be described as follows:
 * - Process 0:
 *     - has 3 integers to send, as follows, it sends:
 *         - to process 0: the first integer
 *         - to process 1: the last 2 integers
 *         - to process 2: nothing
 *     - has 2 integers to receive, as follows, it receives:
 *         - from process 0: 1 integer, stores it at the end
 *         - from process 1: nothing
 *         - from process 2: 1 integer, stores it at the beginning
 * - Process 1:
 *     - has 3 integers to send, as follows, it sends:
 *         - nothing to process 0
 *         - nothing to itself
 *         - 3 integers to process 2
 *     - has 2 integers to receive, as follows, it receives:
 *         - 2 integers rom process 0
 *         - nothing from itself
 *         - nothing from process 2
 * - Process 2:
 *     - has 1 integer to send, as follows, it sends:
 *         - 1 integer to process 0
 *         - nothing to process 1
 *         - nothing to itself
 *     - has 3 integers to receive, as follows, it receives:
 *         - nothing from process 0
 *         - 3 integers from process 1
 *         - nothing from itself
 *
 * In addition to the above, it can be visualised as follows:
 *
 * +-----------------------+ +-----------------------+ +-----------------------+
 * |       Process 0       | |       Process 1       | |       Process 2       |
 * +-------+-------+-------+ +-------+-------+-------+ +-------+-------+-------+
 * | Value | Value | Value | | Value | Value | Value |         | Value |
 * |   0   |  100  |  200  | |  300  |  400  |  500  |         |  600  |
 * +-------+-------+-------+ +-------+-------+-------+         +-------+
 *     |       |       |        |        |       |_________________|_______
 *     |       |       |        |        |_________________________|_      |
 *     |       |       |        |______________________________    | |     |
 *     |       |       |_____________________                  |   | |     |
 *     |       |_______________________      |                 |   | |     | 
 *     |   ____________________________|_____|_________________|___| |     |
 *     |__|_____                       |     |                 |     |     | 
 *        |     |                      |     |                 |     |     | 
 *     +-----+-----+                +-----+-----+           +-----+-----+-----+
 *     | 600 |  0  |                | 100 | 200 |           | 300 | 400 | 500 |
 *  +--+-----+-----+--+         +---+-----+-----+-+         +-----+-----+-----+
 *  |    Process 0    |         |    Process 1    |         |    Process 2    |
 *  +-----------------+         +-----------------+         +-----------------+
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Get number of processes and check that 3 processes are used
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 3)
    {
        printf("This application is meant to be run with 3 MPI processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // Define the buffer containing the values to send
    int* buffer_send;
    int buffer_send_length;
    switch(my_rank)
    {
        case 0:
            buffer_send_length = 3;
            buffer_send = (int*)malloc(sizeof(int) * buffer_send_length);
            buffer_send[0] = 0;
            buffer_send[1] = 100;
            buffer_send[2] = 200;
            printf("Process %d, my values = %d, %d, %d.\n", my_rank, buffer_send[0], buffer_send[1], buffer_send[2]);
            break;
        case 1:
            buffer_send_length = 3;
            buffer_send = (int*)malloc(sizeof(int) * buffer_send_length);
            buffer_send[0] = 300;
            buffer_send[1] = 400;
            buffer_send[2] = 500;
            printf("Process %d, my values = %d, %d, %d.\n", my_rank, buffer_send[0], buffer_send[1], buffer_send[2]);
            break;
        case 2:
            buffer_send_length = 1;
            buffer_send = (int*)malloc(sizeof(int) * buffer_send_length);
            buffer_send[0] = 600;
            printf("Process %d, my value = %d.\n", my_rank, buffer_send[0]);
            break;
    }

    // Define my counts for sending (how many integers do I send to each process?)
    int counts_send[3];
    switch(my_rank)
    {
        case 0:
            counts_send[0] = 1;
            counts_send[1] = 2;
            counts_send[2] = 0;
            break;
        case 1:
            counts_send[0] = 0;
            counts_send[1] = 0;
            counts_send[2] = 3;
            break;
        case 2:
            counts_send[0] = 1;
            counts_send[1] = 0;
            counts_send[2] = 0;
            break;
    }

    // Define my displacements for sending (where is located in the buffer each message to send?)
    int displacements_send[3];
    switch(my_rank)
    {
        case 0:
            displacements_send[0] = 0;
            displacements_send[1] = 1;
            displacements_send[2] = 0;
            break;
        case 1:
            displacements_send[0] = 0;
            displacements_send[1] = 0;
            displacements_send[2] = 0;
            break;
        case 2:
            displacements_send[0] = 0;
            displacements_send[1] = 0;
            displacements_send[2] = 0;
            break;
    }

    // Define the buffer for reception
    int* buffer_recv;
    int buffer_recv_length;
    switch(my_rank)
    {
        case 0:
            buffer_recv_length = 2;
            buffer_recv = malloc(sizeof(int) * buffer_recv_length);
            break;
        case 1:
            buffer_recv_length = 2;
            buffer_recv = malloc(sizeof(int) * buffer_recv_length);
            break;
        case 2:
            buffer_recv_length = 3;
            buffer_recv = malloc(sizeof(int) * buffer_recv_length);
            break;
    }

    // Define my counts for receiving (how many integers do I receive from each process?)
    int counts_recv[3];
    switch(my_rank)
    {
        case 0:
            counts_recv[0] = 1;
            counts_recv[1] = 0;
            counts_recv[2] = 1;
            break;
        case 1:
            counts_recv[0] = 2;
            counts_recv[1] = 0;
            counts_recv[2] = 0;
            break;
        case 2:
            counts_recv[0] = 0;
            counts_recv[1] = 3;
            counts_recv[2] = 0;
            break;
    }

    // Define my displacements for reception (where to store in buffer each message received?)
    int displacements_recv[3];
    switch(my_rank)
    {
        case 0:
            displacements_recv[0] = 1;
            displacements_recv[1] = 0;
            displacements_recv[2] = 0;
            break;
        case 1:
            displacements_recv[0] = 0;
            displacements_recv[1] = 0;
            displacements_recv[2] = 0;
            break;
        case 2:
            displacements_recv[0] = 0;
            displacements_recv[1] = 0;
            displacements_recv[2] = 0;
            break;
    }

    MPI_Request request;
    MPI_Ialltoallv(buffer_send, counts_send, displacements_send, MPI_INT, buffer_recv, counts_recv, displacements_recv, MPI_INT, MPI_COMM_WORLD, &request);

    // Do another job while the non-blocking all to all progresses
    printf("[Process %d] The non-blocking variable all to all is in progress.\n", my_rank);

    MPI_Wait(&request, MPI_STATUS_IGNORE);    
    printf("Values received on process %d:", my_rank);
    for(int i = 0; i < buffer_recv_length; i++)
    {
        printf(" %d", buffer_recv[i]);
    }
    printf("\n");

    free(buffer_send);
    free(buffer_recv);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
