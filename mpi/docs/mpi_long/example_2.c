#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate an array of long ints between 2 MPI
 * processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends an array of long ints to the latter, which
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
			// Send the long ints
			long int longIntsToSend[2] = { 12345, 67890 };
			printf("[MPI process %d] I send long ints: %ld and %ld.\n", my_rank, longIntsToSend[0], longIntsToSend[1]);
			MPI_Ssend(longIntsToSend, 2, MPI_LONG, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			// Receive the long ints
			long int longIntsReceived[2];
			MPI_Recv(longIntsReceived, 2, MPI_LONG, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("[MPI process %d] I received long ints: %ld and %ld.\n", my_rank, longIntsReceived[0], longIntsReceived[1]);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
