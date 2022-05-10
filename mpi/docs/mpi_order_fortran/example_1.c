#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to indicate a FORTRAN-like memory ordering.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver. These two MPI processes will exchange a message made of six
 * integers. These integers turn out to be a subarray of a 2D array held on the
 * sender. An MPI subarray type will be created to extract that subarray and
 * send it, and the memory layout specified will be that of FORTRAN.
 *
 * In the visualisation below, the leftmost dim (dim[0]) has been chosen as
 * being the one having consecutive elements in memory.
 *
 *                                                 The subarray we
 *               The full array                      want to send
 *
 *       +---------- dim[0] ------->         +--------- dim[0] ------->
 *       | +-----+-----+-----+-----+         | +-----+-----+-----+-----+ 
 *       | |  0  |  1  |  2  |  3  |         | |  -  |  -  |  -  |  -  | ^ Start point in
 *       | +-----+-----+-----+-----+         | +-----+-----+-----+-----+ | dim[1] = 2
 *       | |  4  |  5  |  6  |  7  |         | |  -  |  -  |  -  |  -  | V
 * dim[1]| +-----+-----+-----+-----+   dim[1]| +-----+-----+-----+-----+
 *       | |  8  |  9  |  10 |  11 |         | |  -  |  9  |  10 |  11 | ^ Element count
 *       | +-----+-----+-----+-----+         | +-----+-----+-----+-----+ | in dim[1] = 2
 *       | |  12 |  13 |  14 |  15 |         | |  -  |  13 |  14 |  15 | V
 *       V +-----+-----+-----+-----+         V +-----+-----+-----+-----+
 *                                              <---> <--------------->
 *                                        Start point   Element count
 *                                      in dim[0] = 1   in dim[0] = 3
 *
 * In brief; the 3x2 subarray to send starts at [1;2] in the 4x4 full array.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get the number of processes and check only 2 processes are used
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 2)
	{
		printf("This application is meant to be run with 2 processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get my rank and do the corresponding job
	enum role_ranks { SENDER, RECEIVER };
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	switch(my_rank)
	{
		case SENDER:
		{
			// Declare the full array
			int full_array[4][4];
			for(int i = 0; i < 4; i++)
			{
				for(int j = 0; j < 4; j++)
				{
					full_array[i][j] = i * 4 + j;
				}
			}

			// Create the subarray datatype
			MPI_Datatype subarray_type;
			int dimensions_full_array[2] = { 4, 4 };
			int dimensions_subarray[2] = { 3, 2 };
			int start_coordinates[2] = { 1, 2 };
			MPI_Type_create_subarray(2,  dimensions_full_array, dimensions_subarray, start_coordinates, MPI_ORDER_FORTRAN, MPI_INT, &subarray_type);
			MPI_Type_commit(&subarray_type);

			// Send the message
			printf("MPI process %d sends:\n-  -  -  -\n-  -  -  -\n-  %d %d %d\n- %d %d %d\n", my_rank, full_array[2][1], full_array[2][2], full_array[2][3], full_array[3][1], full_array[3][2], full_array[3][3]);
			MPI_Send(full_array, 1, subarray_type, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			// Receive the message
			int received[6];
			MPI_Recv(&received, 6, MPI_INT, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("MPI process %d receives:\n%d %d %d %d %d %d\n", my_rank, received[0], received[1], received[2], received[3], received[4], received[5]);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
