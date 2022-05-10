#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to check multiple non-blocking routines for the 
 * completion of one of them.
 * @details This program is meant to be run with 3 processes: a sender and two
 * receivers. The sender emits 2 messages in a non-blocking fashion. It then
 * tests the corresponding request handlers to see which operation finished. It
 * then repeats the process to check the other operation to finish.
 * This application covers two cases:
 * - None of the underlying non-blocking operations is complete yet.
 * - One of the underlying non-blocking operations is complete.
 * 
 *                  +---------------+---------------+
 *                  | MPI_Recv from | MPI_Recv from |
 *                  | process 1 is  | process 2 is  |
 *                  |    complete   |    complete   |
 * +----------------+---------------+---------------+
 * | MPI_Testany #1 |               |               |
 * | MPI_Testany #2 |       X       |               |
 * | MPI_Testany #3 |               |       X       |
 * +----------------+---------------+---------------+
 *
 * (Note to the reader: the use of MPI_Barrier is only to control the execution
 * flow to make sure it exposes the different cases mentioned above when the
 * MPI_Testany are called.)
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get the number of processes and check only 3 processes are used
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 3)
	{
		printf("This application is meant to be run with 3 processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	if(my_rank == 0)
	{
		// The "master" MPI process sends the messages.
		int buffer[2] = {12345, 67890};
		int count = 2;
		MPI_Request requests[2];
		int recipient_rank_of_request[2];

		// Send first message to process 1
		printf("[Process %d] Sends %d to process 1.\n", my_rank, buffer[0]);
		MPI_Issend(&buffer[0], 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &requests[0]);
		recipient_rank_of_request[0] = 1;

		// Send second message to process 2
		printf("[Process %d] Sends %d to process 2.\n", my_rank, buffer[1]);
		MPI_Issend(&buffer[1], 1, MPI_INT, 2, 0, MPI_COMM_WORLD, &requests[1]);
		recipient_rank_of_request[1] = 2;

		int index;
		int ready;

		// Check if one of the requests finished
		MPI_Testany(count, requests, &index, &ready, MPI_STATUS_IGNORE);
		if(ready)
			printf("MPI_Issend to process %d completed.\n", index + 1);
		else
			printf("None of the MPI_Issend completed for now.\n");

		// Tell receivers they can now issue the first MPI_Recv.
		MPI_Barrier(MPI_COMM_WORLD);

		// Receivers tell us the MPI_Recv is complete.
		MPI_Barrier(MPI_COMM_WORLD);

		// Check if one of the requests finished
		MPI_Testany(count, requests, &index, &ready, MPI_STATUS_IGNORE);
		if(ready)
			printf("MPI_Issend to process %d completed.\n", recipient_rank_of_request[index]);
		else
			printf("None of the MPI_Issend completed for now.\n");

		// Tell receivers they can now issue the second MPI_Recv.
		MPI_Barrier(MPI_COMM_WORLD);

		// Receivers tell us the second MPI_Recv is complete.
		MPI_Barrier(MPI_COMM_WORLD);

		// Check if one of the requests finished
		MPI_Testany(count, requests, &index, &ready, MPI_STATUS_IGNORE);
		if(ready)
			printf("MPI_Issend to process %d completed.\n", recipient_rank_of_request[index]);
		else
			printf("None of the MPI_Issend completed for now.\n");
	}
	else
	{
		// The "slave" MPI processes receive the messages.

		// Wait until the first MPI_Testany is issued.
		MPI_Barrier(MPI_COMM_WORLD);

		if(my_rank == 1)
		{
			int received;
			MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("[Process %d] Received value %d.\n", my_rank, received);
		}

		// Tell that the first MPI_Recv is issued.
		MPI_Barrier(MPI_COMM_WORLD);

		// Wait until the second MPI_Testany is issued.
		MPI_Barrier(MPI_COMM_WORLD);

		if(my_rank == 2)
		{
			int received;
			MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("[Process %d] Received value %d.\n", my_rank, received);
		}

		// Wait for the second MPI_Recv to be issued.
		MPI_Barrier(MPI_COMM_WORLD);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
