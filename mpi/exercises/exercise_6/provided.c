#include <stdio.h>
#include <mpi.h>

/**
 * @brief Exercise to replace blocking with non-blocking communications.
 * @details This application consists of 2 MPI processes, a sender and a
 * receiver. They exchange an integer using a blocking send and a blocking
 * receive. The aim is to replace these with their non-blocking counterparts and
 * handle the non-blocking communication.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(comm_size != 2)
	{
		printf("This application must be run with 2 MPI processes.\n");
		MPI_Abort(MPI_COMM_WORLD, -1);
	}

	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	if(my_rank == 0)
	{
		int value_sent = 12345;
		printf("[MPI process %d] I sent value %d.\n", my_rank, value_sent);
		MPI_Send(&value_sent, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
	}
	else
	{
		int value_received = 0;
		MPI_Recv(&value_received, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		printf("[MPI process %d] I received value %d.\n", my_rank, value_received);
	}

	MPI_Finalize();

	return 0;
}