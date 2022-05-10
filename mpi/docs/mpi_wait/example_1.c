#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to wait for the completion of a non-blocking
 * operation.
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

	// Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	if(my_rank == 0)
	{
		// The "master" MPI process sends the message.
		int buffer = 12345;
		printf("MPI process %d sends the value %d.\n", my_rank, buffer);
		MPI_Ssend(&buffer, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
	}
	else
	{
		// The "slave" MPI process receives the message.
		int received;
		MPI_Request request;
		MPI_Irecv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &request);

		// Do some other things while the underlying MPI_Recv progresses.
		printf("MPI process %d issued the MPI_Irecv and moved on printing this message.\n", my_rank);

		// Wait for the MPI_Recv to complete.
		printf("MPI process %d waits for the underlying MPI_Recv to complete.\n", my_rank);
		MPI_Wait(&request, MPI_STATUS_IGNORE);
		printf("The MPI_Wait completed, which means the underlying request (i.e: MPI_Recv) completed too.\n");
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
