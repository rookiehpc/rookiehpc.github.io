#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to receive a message in a non-blocking fashion.
 * @details This application is meant to be run with 2 processes: 1 sender and 1
 * receiver. The receiver immediately issues the MPI_Irecv, then it moves on
 * printing a message while the reception takes place in the meantime. Finally,
 * the receiver waits for the underlying MPI_Recv to print the value received.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get the number of processes
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
			// The "master" MPI process sends the message.
			int buffer = 12345;
			printf("[Process %d] I send the value %d.\n", my_rank, buffer);
			MPI_Ssend(&buffer, 1, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			// The "slave" MPI process receives the message.
			int received;
			MPI_Request request;
			printf("[Process %d] I issue the MPI_Irecv to receive the message as a background task.\n", my_rank);
			MPI_Irecv(&received, 1, MPI_INT, SENDER, 0, MPI_COMM_WORLD, &request);

			// Do other things while the MPI_Irecv completes.
			printf("[Process %d] The MPI_Irecv is issued, I now moved on to print this message.\n", my_rank);

			// Wait for the MPI_Recv to complete.
			MPI_Wait(&request, MPI_STATUS_IGNORE);
			printf("[Process %d] The MPI_Irecv completed, therefore so does the underlying MPI_Recv. I received the value %d.\n", my_rank, received);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
