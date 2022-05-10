#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to use the variable version of an all gather.
 * @details This application is meant to be run with 3 MPI processes. Every MPI
 * process begins with a value, each process will gather all these values and
 * print them. The example is designed to cover all cases:
 * - Different displacements
 * - Different receive counts
 * It can be visualised as follows:
 *
 * +-----------+ +-----------+ +-------------------+ 
 * | Process 0 | | Process 1 | |     Process 2     |
 * +-+-------+-+ +-+-------+-+ +-+-------+-------+-+
 *   | Value |     | Value |     | Value | Value |
 *   |  100  |     |  101  |     |  102  |  103  |
 *   +-------+     +-------+     +-------+-------+
 *      |                |            |     |
 *      |                |            |     |
 *      |                |            |     |
 *      |                |            |     |
 *      |                |            |     |
 *      |                |            |     |
 *   +-----+-----+-----+-----+-----+-----+-----+
 *   | 100 |  0  |  0  | 101 |  0  | 102 | 103 |
 *   +-----+-----+-----+-----+-----+-----+-----+
 *   |               Each process              |
 *   +-----------------------------------------+
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get number of processes and check only 3 processes are used
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

	// Define the receive counts
	int counts[3] = {1, 1, 2};

	// Define the displacements
	int displacements[3] = {0, 3, 5};

	// Buffer in which receive the data collected
	int buffer[7] = {0};

	// Buffer containing our data to send
	int* my_values;
	int my_values_count;

	switch(my_rank)
	{
		case 0:
		{
			// Define my values
			my_values_count = 1;
			my_values = (int*)malloc(sizeof(int) * my_values_count);
			*my_values = 100;
			printf("Value sent by process %d: %d.\n", my_rank, *my_values);
			break;
		}
		case 1:
		{
			// Define my values
			my_values_count = 1;
			my_values = (int*)malloc(sizeof(int) * my_values_count);
			*my_values = 101;
			printf("Value sent by process %d: %d.\n", my_rank, *my_values);
			break;
		}
		case 2:
		{
			// Define my values
			my_values_count = 2;
			my_values = (int*)malloc(sizeof(int) * my_values_count);
			my_values[0] = 102;
			my_values[1] = 103;
			printf("Values sent by process %d: %d and %d.\n", my_rank, my_values[0], my_values[1]);
			break;
		}
	}

	MPI_Allgatherv(my_values, my_values_count, MPI_INT, buffer, counts, displacements, MPI_INT, MPI_COMM_WORLD);

	printf("Values gathered in the buffer on process %d:", my_rank);
	for(int i = 0; i < 7; i++)
	{
		printf(" %d", buffer[i]);
	}
	printf("\n");
	free(my_values);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
