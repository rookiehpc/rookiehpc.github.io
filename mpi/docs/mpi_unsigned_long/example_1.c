#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate a unsigned long int between 2 MPI
 * processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends a unsigned long int to the latter, which
 * prints it.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Check that 2 MPI processes are used.
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 2)
	{
		printf("This application is meant to be run with 2 MPI processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get my rank and do the corresponding job.
	enum role_ranks { SENDER, RECEIVER };
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	switch(my_rank)
	{
		case SENDER:
		{
			// Send the unsigned long int
			unsigned long int unsignedLongIntToSend = 12345;
			printf("[MPI process %d] I send unsigned long int: %lu.\n", my_rank, unsignedLongIntToSend);
			MPI_Ssend(&unsignedLongIntToSend, 1, MPI_UNSIGNED_LONG, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			// Receive the unsigned long int
			unsigned long int unsignedLongIntReceived;
			MPI_Recv(&unsignedLongIntReceived, 1, MPI_UNSIGNED_LONG, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("[MPI process %d] I received unsigned long int: %lu.\n", my_rank, unsignedLongIntReceived);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
