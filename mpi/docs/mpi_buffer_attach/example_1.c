#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to attach a memory buffer to MPI so MPI_Bsend issued
 * use this buffer to copy the messages to send.
 * @details This application requires 2 processes: 1 sender and 1 receiver.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Check that 2 processes are used
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
			// The message to send
			int message = 1234;

			// Allocate enough space to issue the buffered send
			int buffer_size = (MPI_BSEND_OVERHEAD + sizeof(int));
			char* buffer = malloc(buffer_size);

			// Pass the buffer allocated to MPI so it uses it when we issue MPI_Bsend
			MPI_Buffer_attach(buffer, buffer_size);

			// Issue the buffered send
			printf("[Process %d] I send value %d to process %d.\n", my_rank, message, RECEIVER);
			MPI_Bsend(&message, 1, MPI_INT, RECEIVER, 0, MPI_COMM_WORLD);

			// Detach the buffer no-longer used (it will wait for MPI_Bsend message to be sent first)
			MPI_Buffer_detach(&buffer, &buffer_size);
			free(buffer);
			break;
		}
		case RECEIVER:
		{
			int buffer;
			MPI_Recv(&buffer, 1, MPI_INT, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("[Process %d] I received value %d.\n", my_rank, buffer);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
