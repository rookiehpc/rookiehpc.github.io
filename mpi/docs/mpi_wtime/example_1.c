#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use MPI_Wtime.
 * @details This application consists of 4 MPI processes which will forward a
 * message from MPI process 0 onwards. Every MPI process will introduce a
 * latency of 250ms by waiting, using MPI_Wtime to see how much time has been
 * waited already. The job of each MPI process is to simulate this latency, then
 * send a message to the next MPI process, if any. Also, each MPI process will
 * have checked the time before and after its job using MPI_Wtime, and will 
 * print the timing obtained (effectively being the difference between the two
 * timings). The execution flow can be visualised as follows:
 *
 * +------------------------------------------------------------------+ 
 * | MPI process 0                                                    |
 * | | Start clock                                                    |
 * | | Wait 250ms                                                     |
 * | +--------------> MPI process 1                                   |
 * | |                | Start clock                                   |
 * | |                | Wait 250ms                                    |
 * | |                +--------------> MPI process 2                  |
 * | |                |                | Start clock                  |
 * | |                |                | Wait 250ms                   |
 * | |                |                +--------------> MPI process 3 |
 * | |                |                |                | Start clock |
 * | |                |                |                | Wait 250ms  |
 * | V                V                V                V             |
 * +-+----------------+----------------+----------------+-------------+ 
 * |                             MPI BARRIER                          |
 * +-+----------------+----------------+----------------+-------------+
 * | | Stop clock     | Stop clock     | Stop clock     | Stop clock  |
 * | X Print time     X Print time     X Print time     X Print time  |
 * +------------------------------------------------------------------+
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Check that 4 MPI processes are used
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(comm_size != 4)
	{
		printf("This application is meant to be run with 4 MPI processes, not %d.\n", comm_size);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Time to wait before processing, in seconds
	const double waiting_time = 0.25;
	int message = 12345;
	double start;
	double end;

	if(my_rank == 0)
	{
		// If I am the first MPI process, I send the message to the next MPI process and wait for reception
		
		// Job begins for me, check the clock
		start = MPI_Wtime();

		// I simulate the latency
		while(MPI_Wtime() - start < waiting_time)
		{
			// We keep looping until <waiting_time> seconds have elapsed
		}

		MPI_Send(&message, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);		
	}
	else
	{
		// If am not the first MPI process, I receive the message from the previous MPI process first
		MPI_Recv(&message, 1, MPI_INT, my_rank - 1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

		// Job begins for me, check the clock
		start = MPI_Wtime();

		// I simulate the latency
		while(MPI_Wtime() - start < waiting_time)
		{
			// We keep looping until <waiting_time> seconds have elapsed
		}

		// If I am not the last MPI process
		if(my_rank != comm_size - 1)
		{
			// I forward the message to the next MPI process
			MPI_Send(&message, 1, MPI_INT, my_rank + 1, 0, MPI_COMM_WORLD);
		}
	}

	// Wait for the very last one to 
	MPI_Barrier(MPI_COMM_WORLD);
	end = MPI_Wtime();

	printf("[MPI process %d] time elapsed during the job: %.2fs.\n", my_rank, end - start);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
