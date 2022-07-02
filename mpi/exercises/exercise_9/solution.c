#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define ROWS 4
#define COLS 3

/**
 * @brief Solution to the MPI exercise "Odd-even scatter".
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(comm_size != 3)
	{
		printf("This application must be run with 3 processes, not %d.\n", comm_size);
		MPI_Abort(MPI_COMM_WORLD, -1);
	}

	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	if(my_rank == 0)
	{
		// Initialise the array
		int entire_array[ROWS][COLS];
		for(int i = 0; i < ROWS; i++)
		{
			for(int j = 0; j < COLS; j++)
			{
				entire_array[i][j] = i * COLS + j;
				printf("  %2d", entire_array[i][j]);
			}
			printf("\n");
		}
		printf("\n");

		// Create a vector extracting an integer every two integers
		MPI_Datatype BRICK;
		MPI_Type_vector(ROWS * COLS / 2, 1, 2, MPI_INT, &BRICK);

		// Resize the vector so that we can offset them only by the size of an integer
		MPI_Datatype BRICK_RESIZED;
		MPI_Type_create_resized(BRICK, 0, sizeof(int), &BRICK_RESIZED);
		MPI_Type_commit(&BRICK_RESIZED);

		int counts[3] = {0, 1, 1};
		int displacements[3] = {0, 1, 0};

		// Participate to the scatterv, receiving nothing though
		MPI_Scatterv(entire_array, counts, displacements, BRICK_RESIZED, NULL, 0, MPI_INT, 0, MPI_COMM_WORLD);
	}
	else
	{
		int count = ROWS * COLS / 2;
		int my_array[count];

		// Participate to the scatterv, sending nothing though
		MPI_Scatterv(NULL, NULL, NULL, MPI_INT, my_array, count, MPI_INT, 0, MPI_COMM_WORLD);

		// Print what has been received
		printf("Received on MPI process %d:", my_rank);
		for(int i = 0; i < ROWS * COLS / 2; i++)
		{
			printf("  %2d", my_array[i]);
		}
		printf("\n");
	}

	MPI_Finalize();

	return 0;
}