#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to create an homogeneous indexed block MPI datatype.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver. These two MPI processes will exchange a message made of four
 * integers. On the sender, that message is in fact made of the four corners of
 * an array it holds, which will be represented by an MPI indexed type.
 *
 *
 *     Full array          What we want
 *                            to send
 * +-----+-----+-----+  +-----+-----+-----+
 * |  0  |  1  |  2  |  |  0  |  -  |  2  |
 * +-----+-----+-----+  +-----+-----+-----+
 * |  3  |  4  |  5  |  |  -  |  -  |  -  |
 * +-----+-----+-----+  +-----+-----+-----+
 * |  6  |  7  |  8  |  |  6  |  -  |  8  |
 * +-----+-----+-----+  +-----+-----+-----+
 *
 * How to extract these elements with an homogeneous indexed block type:
 *    
 *            +---------------------------------------- displacement for
 *            |                                      block 3: 8 sizeof(int)
 *            |                                               |
 *            +---------------------------- displacement for  |
 *            |                        block 2: 6 sizeof(int) |
 *            |                                   |           |
 *            +---- displacement for              |           |
 *            |  block 2: 2 sizeof(int)           |           |
 *            |           |                       |           |
 *      displacement for  |                       |           |
 * block 1: 0 sizeof(int) |                       |           |
 *            |           |                       |           |
 *            V           V                       V           |
 *            +-----+-----+-----+-----+-----+-----+-----+-----+-----+
 *            |  0  |  -  |  2  |  -  |  -  |  -  |  6  |  -  |  8  |
 *            +-----+-----+-----+-----+-----+-----+-----+-----+-----+
 *             <--->       <--->                   <--->       <--->
 *            block 1     block 2                 block 3     block 4
 *           1 element   1 element               1 element   1 element
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
            MPI_Datatype corner_type;
            MPI_Aint displacements[4] = { 0, 2 * sizeof(int), 6 * sizeof(int), 8 * sizeof(int)};
            MPI_Type_create_hindexed_block(4, 1, displacements, MPI_INT, &corner_type);
            MPI_Type_commit(&corner_type);

            // Send the message
            int buffer[3][3] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };
            printf("MPI process %d sends values:\n%d %d\n%d %d\n", my_rank, buffer[0][0], buffer[0][2], buffer[2][0], buffer[2][2]);
            MPI_Send(buffer, 1, corner_type, RECEIVER, 0, MPI_COMM_WORLD);
            break;
        }
        case RECEIVER:
        {
            // Receive the message
            int received[4];
            MPI_Recv(&received, 4, MPI_INT, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            printf("MPI process %d received values:\n%d %d\n%d %d\n", my_rank, received[0], received[1], received[2], received[3]);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
