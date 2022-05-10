#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to wait on multiple non-blocking routines until at
 * least one of them completes.
 * @details This program is meant to be run with 4 processes: 1 sender and 3
 * receivers. The sender emits 3 messages using non-blocking sends, one to each
 * receiver. It then uses MPI_Waitsome to see which non-blocking routines
 * completed. This application covers multiple cases:
 * - Multiple non-blocking routines completed
 * - Single non-blocking routines completed
 * 
 * The execution flow can be visualised below:
 *
 *                       +-----------+-----------+-----------+
 *                       | Process 1 | Process 2 | Process 3 |
 * +---------------------+-----------+-----------+-----------+
 * | First MPI_Waitsome  |     x     |     x     |           |
 * | Second MPI_Waitsome |           |           |     x     |
 * +---------------------+-----------+-----------+-----------+
 *
 * (Note to readers: the MPI barriers used in this code are present just to make
 * sure that the application always exposes the execution flow depicted above.)
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get the number of processes and check only 4 processes are used
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 4)
	{
		printf("This application is meant to be run with 4 processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	switch(my_rank)
	{
		case 0:
		{
			// The "master" MPI process sends the messages.
			int buffer[3] = {123, 456, 789};
			MPI_Request requests[3];
			int recipient_rank_of_request[3];

			// Send first message to process 1
			printf("[Process %d] Sends %d to process 1.\n", my_rank, buffer[0]);
			MPI_Issend(&buffer[0], 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &requests[0]);
			recipient_rank_of_request[0] = 1;

			// Send second message to process 2
			printf("[Process %d] Sends %d to process 2.\n", my_rank, buffer[1]);
			MPI_Issend(&buffer[1], 1, MPI_INT, 2, 0, MPI_COMM_WORLD, &requests[1]);
			recipient_rank_of_request[1] = 2;

			// Send second message to process 3
			printf("[Process %d] Sends %d to process 3.\n", my_rank, buffer[2]);
			MPI_Issend(&buffer[2], 1, MPI_INT, 3, 0, MPI_COMM_WORLD, &requests[2]);
			recipient_rank_of_request[2] = 3;

			// Barrier to make sure that the sends 1 and 2 are complete by the first MPI_Waitsome
			MPI_Barrier(MPI_COMM_WORLD);

			// Wait for one of non-blocking sends to complete
			int index_count;
			int indices[3];
			MPI_Waitsome(3, requests, &index_count, indices, MPI_STATUSES_IGNORE);
			for(int i = 0; i < index_count; i++)
			{
				printf("[Process %d] First MPI_Waitsome: the non-blocking send to process %d is complete.\n", my_rank, recipient_rank_of_request[indices[i]]);
			}

			// Barrier to make sure that the send 3 is complete by the second MPI_Waitsome
			MPI_Barrier(MPI_COMM_WORLD);

			// Wait for the other non-blocking send to complete
			MPI_Waitsome(3, requests, &index_count, indices, MPI_STATUSES_IGNORE);
			for(int i = 0; i < index_count; i++)
			{
				printf("[Process %d] Second MPI_Waitsome: the non-blocking send to process %d is complete.\n", my_rank, recipient_rank_of_request[indices[i]]);
			}
			break;
		}
		case 3:
		{
			MPI_Barrier(MPI_COMM_WORLD);
			MPI_Barrier(MPI_COMM_WORLD);

			// The last MPI process will wait on the barrier before receiving the message.
			int received;
			MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("[Process %d] Received value %d.\n", my_rank, received);
			break;
		}
		default:
		{
			// The MPI processes 1 and 2 will receive the message, then they wait on the barrier.
			int received;
			MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("[Process %d] Received value %d.\n", my_rank, received);

			MPI_Barrier(MPI_COMM_WORLD);
			MPI_Barrier(MPI_COMM_WORLD);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
