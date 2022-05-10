#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Pair communications between 2 MPI processes sending a message to each other.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Make sure exactly 2 MPI processes are used
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 2)
	{
	    printf("%d MPI processes used, please use 2.\n", size);
	    MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Prepare parameters
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	int buffer_send = (my_rank == 0) ? 12345 : 67890;
	int buffer_recv;
	int tag_send = 0;
	int tag_recv = tag_send;
	int peer = (my_rank == 0) ? 1 : 0;

	// Issue the send + receive at the same time
	printf("MPI process %d sends value %d to MPI process %d.\n", my_rank, buffer_send, peer);
	MPI_Sendrecv(&buffer_send, 1, MPI_INT, peer, tag_send,
		         &buffer_recv, 1, MPI_INT, peer, tag_recv, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	printf("MPI process %d received value %d from MPI process %d.\n", my_rank, buffer_recv, peer);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
