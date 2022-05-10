#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to create a contiguous MPI datatype.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver. These two MPI processes will exchange a message made of two
 * integers. To that end, they each create a datatype representing that layout.
 * The corresponding MPI_Datatype is created, then committed before being used
 * in communications, to eventually be freed.
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

	// Create the datatype
	MPI_Datatype double_int_type;
	MPI_Type_contiguous(2, MPI_INT, &double_int_type);

	// Commit the datatype so we can use it in communications
	MPI_Type_commit(&double_int_type);

	// Get my rank and do the corresponding job
	enum role_ranks { SENDER, RECEIVER };
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	switch(my_rank)
	{
		case SENDER:
		{
			// Send the message
			int buffer_sent[2] = {12345, 67890};
			printf("MPI process %d sends values %d and %d.\n", my_rank, buffer_sent[0], buffer_sent[1]);
			MPI_Send(&buffer_sent, 1, double_int_type, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			// Receive the message
			int received[2];
			MPI_Recv(&received, 1, double_int_type, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("MPI process %d received values: %d and %d.\n", my_rank, received[0], received[1]);
			break;
		}
	}

	// Free the datatype created
	MPI_Type_free(&double_int_type);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
