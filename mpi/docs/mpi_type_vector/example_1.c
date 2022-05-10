#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to create a vector MPI datatype.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver. These two MPI processes will exchange a message made of three
 * integers. On the sender, that message is in fact the middle column of an
 * array it holds, which will be represented by an MPI vector.
 *
 *
 *     Full array          What we want
 *                            to send
 * +-----+-----+-----+  +-----+-----+-----+
 * |  0  |  1  |  2  |  |  -  |  1  |  -  |
 * +-----+-----+-----+  +-----+-----+-----+
 * |  3  |  4  |  5  |  |  -  |  4  |  -  |
 * +-----+-----+-----+  +-----+-----+-----+
 * |  6  |  7  |  8  |  |  -  |  7  |  -  |
 * +-----+-----+-----+  +-----+-----+-----+
 *
 * How to extract a column with a vector type:
 *
 *                 distance between the
 *            start of each block: 3 elements
 *          <---------------> <--------------->
 *         |                 |                 |
 *      start of          start of          start of
 *      block 1           block 2           block 3
 *         |                 |                 |
 *         V                 V                 V
 *   +-----+-----+-----+-----+-----+-----+-----+-----+-----+
 *   |  -  |  1  |  -  |  -  |  4  |  -  |  -  |  7  |  -  |
 *   +-----+-----+-----+-----+-----+-----+-----+-----+-----+
 *          <--->             <--->             <--->
 *         block 1           block 2           block 3
 * 
 * Block length: 1 element
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
			MPI_Datatype column_type;
			MPI_Type_vector(3, 1, 3, MPI_INT, &column_type);
			MPI_Type_commit(&column_type);

			// Send the message
			int buffer[3][3] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };
			MPI_Request request;
			printf("MPI process %d sends values %d, %d and %d.\n", my_rank, buffer[0][1], buffer[1][1], buffer[2][1]);
			MPI_Send(&buffer[0][1], 1, column_type, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			// Receive the message
			int received[3];
			MPI_Recv(&received, 3, MPI_INT, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("MPI process %d received values: %d, %d and %d.\n", my_rank, received[0], received[1], received[2]);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
