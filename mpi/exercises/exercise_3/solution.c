#include <stdio.h>
#include <mpi.h>

/**
 * @brief Solution to the "Shout it to the whole world" exercise.
 **/
int main(int argc, char* argv[])
{
	// 1) Tell MPI to start
	MPI_Init(&argc, &argv);

	// 2) Check that at least 4 MPI processes are used
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(comm_size != 4)
	{
		printf("This application must be run with at least 4 MPI processes, not %d.\n", comm_size);
		MPI_Abort(MPI_COMM_WORLD, -1);
	}

	// 3) Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// 4) If my rank is 0, I am the broadcast emitter
	if(my_rank == 0)
	{
		// 4.1) Print the value to broadcast
		int value_to_send = 12345;
		printf("I am process %d and I broadcast value %d.\n", my_rank, value_to_send);

		// 4.2) Broadcast the value
		MPI_Bcast(&value_to_send, 1, MPI_INT, 0, MPI_COMM_WORLD);
	}
	// 5) Otherwise, I am a broadcast receiver
	else
	{
		// 5.1) Receive the value by broadcast
		int value_received;
		MPI_Bcast(&value_received, 1, MPI_INT, 0, MPI_COMM_WORLD);

		// 5.2) Print the value received by broadcast
		printf("I am process %d and I received value %d via broadcast.\n", my_rank, value_received);
	}

	// 6) Tell MPI to stop
	MPI_Finalize();

	return 0;
}