#include <stdio.h>
#include <mpi.h>

/**
 * @brief Solution to the MPI exercise: "Switch to non-blocking".
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
		MPI_Request request;

		// Launch the non-blocking send
		MPI_Isend(&value_sent, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &request);
		printf("[MPI process %d] I launched the non-blocking send.\n", my_rank);

		// Wait for the non-blocking send to complete
		MPI_Wait(&request, MPI_STATUS_IGNORE);
		printf("[MPI process %d] The wait completed, so I sent value %d.\n", my_rank, value_sent);
	}
	else
	{
		int value_received = 0;
		MPI_Request request;

		// Launch the non-blocking receive
		MPI_Irecv(&value_received, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &request);
		printf("[MPI process %d] I launched the non-blocking receive.\n", my_rank);

		// Wait for the non-blocking send to complete
		MPI_Wait(&request, MPI_STATUS_IGNORE);
		printf("[MPI process %d] The wait completed, so I received value %d.\n", my_rank, value_received);
	}

	MPI_Finalize();

	return 0;
}