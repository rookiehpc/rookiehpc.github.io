#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Demonstrates how to tell MPI to not fill the MPI_Status.
 * @details This application is designed around a simple message send. There are
 * two MPI processes: a sender and a receiver. The receiver will issue an
 * MPI_Recv and tell MPI not to fill an MPI_Status.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Check that only 2 MPI processes are used.
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(comm_size != 2)
	{
		printf("This application is meant to be run with 2 MPI processes, not %d.\n", comm_size);
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
			MPI_Ssend(&buffer_sent, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			int buffer_received;
			MPI_Recv(&buffer_received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("MPI process %d received value: %d.\n", my_rank, buffer_received);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
