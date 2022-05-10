#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to test for the completion of an array of non-blocking
 * operations.
 * @details This application is designed to cover both cases:
 * - Issuing an MPI_Testall when not all the operations tested are complete
 * - Issuing an MPI_Testall when all the operations tested are complete
 *
 * The application execution flow can be visualised below:
 *
 *                  +--------------------+----------------+
 *                  | Not all operations | All operations |
 *                  |  are complete yet  |  are complete  |
 * +----------------+--------------------+----------------+
 * | MPI_Testall #1 |          X         |                |
 * | MPI_Testall #2 |                    |        X       |
 * +----------------+--------------------+----------------+
 *
 * This program is meant to be run with 3 processes: a sender and two
 * receivers.
 *
 * (Note to readers: the use of a barriers is only to guarantee that the
 * application exposes the execution flow depicted above.)
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
		// The "master" MPI process receives the messages.
		int message_1;
		int message_2;
		MPI_Request requests[2];
		int ready;

		MPI_Irecv(&message_1, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &requests[0]);
		MPI_Irecv(&message_2, 1, MPI_INT, 2, 0, MPI_COMM_WORLD, &requests[1]);

		// This MPI_Testall is guaranteed to fail since the corresponding MPI_Ssends have not been issued yet.
		MPI_Testall(2, requests, &ready, MPI_STATUSES_IGNORE);
		if(ready)
			printf("[Process 0] First attempt: both receives are complete.\n");
		else
			printf("[Process 0] First attempt: not both receives are complete yet.\n");

		// We can tell other processes to start sending messages
		MPI_Barrier(MPI_COMM_WORLD);

		// We wait for the other processes to tell us their MPI_Ssend is complete
		MPI_Barrier(MPI_COMM_WORLD);

		// This MPI_Testall is guaranteed to succeed since the corresponding MPI_Ssends are all complete.
		MPI_Testall(2, requests, &ready, MPI_STATUSES_IGNORE);
		if(ready)
			printf("[Process 0] Second attempt: both receives are complete.\n");
		else
			printf("[Process 0] Second attempt: not both receives are complete yet.\n");
	}
	else
	{
		// The "slave" MPI processes send the messages.
		int message = (my_rank == 1) ? 12345 : 67890;

		// Wait for the MPI_Testall #1 to be done.
		MPI_Barrier(MPI_COMM_WORLD);

		MPI_Ssend(&message, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);

		// Tell the sender it can now issue the MPI_Testall #2.
		MPI_Barrier(MPI_COMM_WORLD);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
