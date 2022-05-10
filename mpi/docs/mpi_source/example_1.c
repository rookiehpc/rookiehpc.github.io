#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Display the source contained in the MPI_Status.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	if(my_rank == 0)
	{
		// The "master" MPI process issues the MPI_Bsend.
		int buffer_sent = 12345;
		printf("MPI process %d sends value %d.\n", my_rank, buffer_sent);
		MPI_Ssend(&buffer_sent, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
	}
	else
	{
		// The "slave" MPI process receives the message.
		int buffer_received;
		MPI_Status status;
		MPI_Recv(&buffer_received, 1, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);
		printf("MPI process %d received value %d from rank %d.\n", my_rank, buffer_received, status.MPI_SOURCE);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
