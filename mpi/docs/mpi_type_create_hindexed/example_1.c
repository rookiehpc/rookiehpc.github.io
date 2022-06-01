#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to create a heterogeneous indexed MPI datatype.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver. These two MPI processes will exchange a message made of six
 * integers. On the sender, that message is in fact the lower triangle of an
 * array it holds, which will be represented by an MPI indexed type.
 *
 *
 *     Full array          What we want
 *                            to send
 * +-----+-----+-----+  +-----+-----+-----+
 * |  0  |  1  |  2  |  |  0  |  -  |  -  |
 * +-----+-----+-----+  +-----+-----+-----+
 * |  3  |  4  |  5  |  |  3  |  4  |  -  |
 * +-----+-----+-----+  +-----+-----+-----+
 * |  6  |  7  |  8  |  |  6  |  7  |  8  |
 * +-----+-----+-----+  +-----+-----+-----+
 *
 * How to extract the lower triangle with a heterogeneous indexed type:
 *
 *   
 *        +---------------------------- displacement for
 *        |                          block 2: 6 sizeof(int)
 *        |                                   |
 *        +---------- displacement for        |
 *        |        block 2: 3 sizeof(int)     |
 *        |                 |                 |
 *  displacement for        |                 |
 *    block 1: 0            |                 |
 *        |                 |                 |
 *        V                 V                 V
 *        +-----+-----+-----+-----+-----+-----+-----+-----+-----+
 *        |  0  |  -  |  -  |  3  |  4  |  -  |  6  |  7  |  8  |
 *        +-----+-----+-----+-----+-----+-----+-----+-----+-----+
 *         <--->             <--------->       <--------------->
 *        block 1              block 2              block 3
 *       1 element            2 elements           3 elements
 * 
 * Element: MPI_INT
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
    enum rank_roles { SENDER, RECEIVER };
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    switch(my_rank)
    {
        case SENDER:
        {
            // Create the datatype
            MPI_Datatype triangle_type;
            int lengths[3] = { 1, 2, 3 };
            MPI_Aint displacements[3] = { 0, 3 * sizeof(int), 6 * sizeof(int) };
            MPI_Type_create_hindexed(3, lengths, displacements, MPI_INT, &triangle_type);
            MPI_Type_commit(&triangle_type);

            // Send the message
            int buffer[3][3] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };
            printf("MPI process %d sends values:\n%d\n%d %d\n%d %d %d\n", my_rank, buffer[0][0], buffer[1][0], buffer[1][1], buffer[2][0], buffer[2][1], buffer[2][2]);
            MPI_Send(buffer, 1, triangle_type, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receive the message
            int received[6];
            MPI_Recv(&received, 6, MPI_INT, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("MPI process %d received values:\n%d\n%d %d\n%d %d %d\n", my_rank, received[0], received[1], received[2], received[3], received[4], received[5]);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
