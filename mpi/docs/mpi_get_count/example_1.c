#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to retrieve the number of elements in a message
 * received.
 * @details This application is meant to be run with 2 processes: a sender and
 * a receiver.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Size of the default communicator
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
			int buffer[3] = {0};
			MPI_Send(buffer, 3, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			// Receive the buffer
			int buffer[3];
			MPI_Status status;
			MPI_Recv(buffer, 3, MPI_INT, SENDER, 0, MPI_COMM_WORLD, &status);

			// Retrieve the number of elements (should be 3)
			int count;
			MPI_Get_count(&status, MPI_INT, &count);
			printf("Number of elements retrieved from the message received: %d.\n", count);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}