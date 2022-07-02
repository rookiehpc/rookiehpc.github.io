#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief This exercise is to find the bug in a simple send-receive.
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
		int buffer_size = sizeof(int);
		char* buffer = (char*)malloc(buffer_size);
		MPI_Buffer_attach(buffer, buffer_size);

		printf("[MPI process %d] I send value %d.\n", my_rank, value_sent);
		MPI_Bsend(&value_sent, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);

		MPI_Buffer_detach(&buffer, &buffer_size);
		free(buffer);
	}
	else
	{
		int value_received;
		MPI_Recv(&value_received, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		printf("[MPI process %d] I received value %d.\n", my_rank, value_received);
	}

	MPI_Finalize();

	return 0;
}