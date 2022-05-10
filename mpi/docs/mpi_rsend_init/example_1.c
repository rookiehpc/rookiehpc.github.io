#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Sends a message as soon as possible in a blocking synchronous fashion.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver.
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
			// Prepare the ready send
			int buffer_sent;
			MPI_Request request;
			MPI_Rsend_init(&buffer_sent, 1, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD, &request);

			for(int i = 0; i < 3; i++)
			{
				printf("MPI process %d hits the barrier to wait for the matching MPI_Recv to be posted.\n", my_rank);
				MPI_Barrier(MPI_COMM_WORLD);
				printf("The barrier unlocked, which means the MPI_Recv is already posted so the ready send can be issued.\n");
				
				printf("MPI process %d sends value %d for message %d.\n", my_rank, buffer_sent, i);
				// Launch the ready send
				MPI_Start(&request);
				// Wait for the ready send to complete
				MPI_Wait(&request, MPI_STATUS_IGNORE);
			}
			break;
		}
		case RECEIVER:
		{
			int received;
			MPI_Request request;
			for(int i = 0; i < 3; i++)
			{
				MPI_Irecv(&received, 1, MPI_INT, SENDER, 0, MPI_COMM_WORLD, &request);

				printf("MPI process %d issued the MPI_Irecv, moved on and hit the barrier.\n", my_rank);
				MPI_Barrier(MPI_COMM_WORLD);

				// Wait for the underlying MPI_Recv to complete.
				MPI_Wait(&request, MPI_STATUS_IGNORE);
				printf("MPI process %d receives value %d for message %d.\n", my_rank, received, i);
			}
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
