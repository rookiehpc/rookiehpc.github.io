#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate an array of unsigned long long ints
 * between 2 MPI processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends an array of unsigned long long ints to the
 * latter, which prints it.
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
			// Send the unsigned long long ints
			unsigned long long int unsignedLongLongIntsToSend[2] = { 12345, 67890 };
			printf("[MPI process %d] I send unsigned long long ints: %llu and %llu.\n", my_rank, unsignedLongLongIntsToSend[0], unsignedLongLongIntsToSend[1]);
			MPI_Ssend(unsignedLongLongIntsToSend, 2, MPI_UNSIGNED_LONG_LONG, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			// Receive the unsigned long ints
			unsigned long long int unsignedLongLongIntsReceived[2];
			MPI_Recv(unsignedLongLongIntsReceived, 2, MPI_UNSIGNED_LONG_LONG, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("[MPI process %d] I received unsigned long long ints: %llu and %llu.\n", my_rank, unsignedLongLongIntsReceived[0], unsignedLongLongIntsReceived[1]);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
