#include <stdio.h>
#include <mpi.h>

/**
 * @brief Solution to the simple send-receive in MPI.
 **/
int main(int argc, char* argv[])
{
	// 1) Tell MPI to start
	MPI_Init(&argc, &argv);

	// 2) Check that the application is run with 2 MPI processes
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(comm_size != 2)
	{
		printf("This application must be run with 2 MPI processes, not %d.\n", comm_size);
		MPI_Abort(MPI_COMM_WORLD, -1);
	}

	// 3) Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// 4) If my rank is 0, I am the sender
	if(my_rank == 0)
	{
		// 4.1) Print the value to send
		int value_to_send = 12345;
		printf("I am process %d and I send value %d.\n", my_rank, value_to_send);

		// 4.2) Send the value
		MPI_Send(&value_to_send, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
	}	
	// 5) If my rank is 1, I am the receiver
	else
	{
		// 5.1) Receive the value
		int value_received = 0;
		MPI_Recv(&value_received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

		// 5.2) Print the value received
		printf("I am process %d and I received value %d.\n", my_rank, value_received);
	}

	// 6) Tell MPI to stop
	MPI_Finalize();

	return 0;
}