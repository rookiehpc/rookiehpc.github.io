#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use an extended variable all to all.
 * @details This application is meant to be run with 3 MPI processes. Each
 * process has an arbitrary number of elements to send and receive, at different
 * positions and of different types. To demonstrate the great flexibility of the
 * MPI_Alltoallw routine, the data exchange designed is rather irregular, so it
 * is extra detailed in this description.
 * 
 * It can be described as follows:
 * - Process 0:
 *     - has 3 elements to send, as follows, it sends:
 *         - to process 0: 1 integer
 *         - to process 1: 2 doubles
 *         - to process 2: nothing
 *     - has 2 elements to receive, as follows, it receives:
 *         - from process 0: 1 integer, stores it at the end
 *         - from process 1: nothing
 *         - from process 2: 1 double, stores it at the beginning
 * - Process 1:
 *     - has 3 elements to send, as follows, it sends:
 *         - nothing to process 0
 *         - the last character to process 1
 *         - the first 2 characters to process 2
 *     - has 2 elements to receive, as follows, it receives:
 *         - from process 0: 2 doubles, stores it at the beginning
 *         - from process 1: 1 character, stores it at the end
 *         - nothing from process 2
 * - Process 2:
 *     - has 1 element to send, as follows, it sends:
 *         - 1 double to process 0
 *         - nothing to process 1
 *         - nothing to itself
 *     - has 3 elements to receive, as follows, it receives:
 *         - nothing from process 0
 *         - from process 1: 2 characters
 *         - nothing from itself
 *
 * In addition to the above, it can be visualised as follows:
 *
 * +-----------------------+ +-----------------------+ +-----------------------+
 * |       Process 0       | |       Process 1       | |       Process 2       |
 * +-------+-------+-------+ +-------+-------+-------+ +-------+-------+-------+
 * | Value | Value | Value | | Value | Value | Value |         | Value |
 * |   1   |  0.0  |  1.0  | |  'a'  |  'b'  |  'c'  |         |  2.0  |
 * +-------+-------+-------+ +-------+-------+-------+         +-------+
 *     |       |       |        |        |       |                 |
 *     |       |       |        |        |_______|_________________|____  
 *     |       |       |        |________________|________________ |    | 
 *     |       |       |_________________        |                ||    | 
 *     |       |___________________      |      _|                ||    | 
 *     |   ________________________|_____|_____|__________________||    | 
 *     |__|_____                   |     |     |                  |     | 
 *        |     |                  |     |     |                  |     | 
 *     +-----+-----+            +-----+-----+-----+            +-----+-----+
 *     | 2.0 |  1  |            | 0.0 | 1.0 | 'c' +            | 'a' | 'b' |
 *  +--+-----+-----+--+         +-----+-----+-----+         +--+-----+-----+--+
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
    char* buffer_send;
    switch(my_rank)
    {
        case 0:
        {
            int element0 = 1;
            double element1 = 0.0;
            double element2 = 1.0;
            buffer_send = (char*)malloc(sizeof(int) + sizeof(double) + sizeof(double));
            memcpy(buffer_send, &element0, sizeof(int));
            memcpy(buffer_send + sizeof(int), &element1, sizeof(double));
            memcpy(buffer_send + sizeof(int) + sizeof(double), &element2, sizeof(double));
            printf("Process %d, my values = %d, %f, %f.\n", my_rank, *((int*)buffer_send),
                                                                     *((double*)(buffer_send + sizeof(int))),
                                                                     *((double*)(buffer_send + sizeof(int) + sizeof(double))));
            break;
        }
        case 1:
        {
            char element0 = 'a';
            char element1 = 'b';
            char element2 = 'c';
            buffer_send = (char*)malloc(sizeof(element0) + sizeof(element1) + sizeof(element2));
            memcpy(buffer_send, &element0, sizeof(char));
            memcpy(buffer_send + sizeof(char), &element1, sizeof(char));
            memcpy(buffer_send + sizeof(char) + sizeof(char), &element2, sizeof(char));
            printf("Process %d, my values = %c, %c, %c.\n", my_rank, *(char*)(buffer_send),
                                                                     *(char*)(buffer_send + sizeof(element0)),
                                                                     *(char*)(buffer_send + sizeof(element0) + sizeof(element1)));
            break;
        }
        case 2:
        {
            double element0 = 2.0;
            buffer_send = (char*)malloc(sizeof(element0));
            memcpy(buffer_send, &element0, sizeof(double));
            printf("Process %d, my value = %f.\n", my_rank, *(double*)buffer_send);
            break;
        }
    }

    // Define my counts for sending (how many elements do I send to each process?)
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
            counts_send[1] = 1;
            counts_send[2] = 2;
            break;
        case 2:
            counts_send[0] = 1;
            counts_send[1] = 0;
            counts_send[2] = 0;
            break;
    }

    // Define my displacements for sending (where is located in the buffer each message to send?)
    // Unlike MPI_Alltoall and MPI_Alltoallv, the displacement in now expressed in bytes
    int displacements_send[3];
    switch(my_rank)
    {
        case 0:
            displacements_send[0] = 0;
            displacements_send[1] = sizeof(int);
            displacements_send[2] = 0; // Not used
            break;
        case 1:
            displacements_send[0] = 0; // Not used
            displacements_send[1] = sizeof(char) + sizeof(char);
            displacements_send[2] = 0;
            break;
        case 2:
            displacements_send[0] = 0;
            displacements_send[1] = 0; // Not used
            displacements_send[2] = 0; // Not used
            break;
    }

    // Define the type of the elements to send to each process
    MPI_Datatype datatypes_send[3];
    switch(my_rank)
    {
        case 0:
            datatypes_send[0] = MPI_INT;
            datatypes_send[1] = MPI_DOUBLE;
            datatypes_send[2] = MPI_INT; // Not used
            break;
        case 1:
            datatypes_send[0] = MPI_CHAR; // Not used
            datatypes_send[1] = MPI_CHAR;
            datatypes_send[2] = MPI_CHAR;
            break;
        case 2:
            datatypes_send[0] = MPI_DOUBLE;
            datatypes_send[1] = MPI_DOUBLE; // Not used
            datatypes_send[2] = MPI_DOUBLE; // Not used
            break;
    }

    // Define the buffer for reception
    char* buffer_recv;
    switch(my_rank)
    {
        case 0:
            buffer_recv = (char*)malloc(sizeof(int) + sizeof(double));
            break;
        case 1:
            buffer_recv = (char*)malloc(sizeof(double) + sizeof(double) + sizeof(char));
            break;
        case 2:
            buffer_recv = (char*)malloc(sizeof(char) + sizeof(char));
            break;
    }

    // Define my counts for receiving (how many elements do I receive from each process?)
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
            counts_recv[1] = 1;
            counts_recv[2] = 0;
            break;
        case 2:
            counts_recv[0] = 0;
            counts_recv[1] = 2;
            counts_recv[2] = 0;
            break;
    }

    // Define my displacements for reception (where to store in buffer each message received?)
    int displacements_recv[3];
    switch(my_rank)
    {
        case 0:
            displacements_recv[0] = sizeof(double);
            displacements_recv[1] = 0; // Not used
            displacements_recv[2] = 0;
            break;
        case 1:
            displacements_recv[0] = 0;
            displacements_recv[1] = sizeof(double) + sizeof(double);
            displacements_recv[2] = 0; // Not used
            break;
        case 2:
            displacements_recv[0] = 0; // Not used
            displacements_recv[1] = 0;
            displacements_recv[2] = 0; // Not used
            break;
    }

    // Define the type of the elements to receive from each process
    MPI_Datatype datatypes_recv[3];
    switch(my_rank)
    {
        case 0:
            datatypes_recv[0] = MPI_INT;
            datatypes_recv[1] = MPI_INT; // Not used
            datatypes_recv[2] = MPI_DOUBLE;
            break;
        case 1:
            datatypes_recv[0] = MPI_DOUBLE;
            datatypes_recv[1] = MPI_CHAR;
            datatypes_recv[2] = MPI_DOUBLE; // Not used
            break;
        case 2:
            datatypes_recv[0] = MPI_CHAR; // Not used
            datatypes_recv[1] = MPI_CHAR;
            datatypes_recv[2] = MPI_CHAR; // Not used
            break;
    }

    MPI_Alltoallw(buffer_send, counts_send, displacements_send, datatypes_send, buffer_recv, counts_recv, displacements_recv, datatypes_recv, MPI_COMM_WORLD);
    
    printf("Values received on process %d:", my_rank);
    switch(my_rank)
    {
        case 0:
            printf(" %f %d", *(double*)buffer_recv, *(int*)(buffer_recv + sizeof(double)));
            break;
        case 1:
            printf(" %f %f %c", *(double*)buffer_recv, *(double*)(buffer_recv + sizeof(double)), *(char*)(buffer_recv + sizeof(double) + sizeof(double)));
            break;
        case 2:
            printf(" %c %c", *(char*)buffer_recv, *(char*)(buffer_recv + sizeof(char)));
            break;
    }
    printf("\n");

    free(buffer_send);
    free(buffer_recv);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
