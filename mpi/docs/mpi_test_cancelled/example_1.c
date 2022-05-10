#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to check whether a request was cancelled.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver. The sender issues a non-blocking send to communicate the value
 * 12345 to the receiver, and cancels it with MPI_Cancel. Both processes
 * check whether the MPI_Cancel happened before or after the message
 * could be exchanged by using MPI_Test_cancelled.
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
			
			// Cancel that request
			MPI_Cancel(&request);

			// The request is marked for cancellation, but the MPI_Cancel operation is local therefore the MPI_Wait is still needed
			MPI_Status status;
			MPI_Wait(&request, &status);

			// Check whether the underlying communication had already taken place
			int flag;
			MPI_Test_cancelled(&status, &flag);

			if(flag)
			{
				// Successful cancellation
				printf("MPI process %d: the cancellation happened before I could send the message, therefore nothing was sent.\n", my_rank);
			}
			else
			{
				// Successful communication
				printf("MPI process %d: the cancellation happened after I sent the message containing value %d.\n", my_rank, buffer_sent);
			}
			break;
		}
		case RECEIVER:
		{
			MPI_Status status;
			int received = 0;
			MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);

			// Check whether the underlying communication had already taken place
			int flag;
			MPI_Test_cancelled(&status, &flag);

			if(flag)
			{
				// Successful cancellation
				printf("MPI process %d: the cancellation happened before I sent the message, therefore I received nothing and my buffer still contains its original value of %d.\n", my_rank, received);
			}
			else
			{
				// Successful communication
				printf("MPI process %d: the cancellation happened after MPI process %d sent the message, therefore I received value %d as normal.\n", my_rank, SENDER, received);
			}

			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
