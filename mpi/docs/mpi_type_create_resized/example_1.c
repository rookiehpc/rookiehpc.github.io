#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to resize an MPI datatype.
 * @details This program is meant to be run with 3 processes. The MPI process 0
 * holds a 2D array of 12 integers; 4 rows times 3 columns, and scatters columns
 * across all three MPI processes as illustrated below:
 *
 *  Elements of a row
 * <----------------->
 * +-----+-----+-----+ ^
 * |  0  |  1  |  2  | |
 * +-----+-----+-----+ |
 * |  3  |  4  |  5  | | Elements of a column
 * +-----+-----+-----+ | 
 * |  6  |  7  |  8  | |
 * +-----+-----+-----+ |
 * |  9  |  10 |  11 | |
 * +-----+-----+-----+ v
 *
 * MPI process 0     MPI process 1     MPI process 2
 *    +-----+           +-----+           +-----+
 *    |  0  |           |  1  |           |  2  |
 *    +-----+           +-----+           +-----+
 *    |  3  |           |  4  |           |  5  |
 *    +-----+           +-----+           +-----+
 *    |  6  |           |  7  |           |  8  |
 *    +-----+           +-----+           +-----+
 *    |  9  |           |  10 |           |  11 |
 *    +-----+           +-----+           +-----+
 *
 * The global array held on MPI process 0 has been declared such that elements
 * of the same row are contiguous in memory, in other words, here is the memory
 * layout of this global array:
 *
 * +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 * |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  10 |  11 |
 * +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 *
 * However the objective here is to scatter columns across the 3 MPI processes.
 * If we identify columns with letters, here is how they are scattered in
 * memory:
 *
 * +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 * |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |
 * +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 *
 * To extract a column, we need the following vector:
 * +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 * |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |
 * +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 * <----->           <----->           <----->           <----->
 * 1st block        2nd block         3rd block        4th block
 *      
 * ^                  <--------------->                        ^
 * | Start of          Stride: distance                 End of |
 * | 1st block       between the start of           last block |
 * |                two consecutive blocks                     |
 * |                                                           |
 * <----------------------------------------------------------->
 *          Extent, as calculated by MPI_Type_vector
 *
 * 
 * When a vector is repeated, as part of a MPI collective for instance, the
 * vector extent is used to determine the start position of the next vector. 
 * This vector extent is calculated by MPI_Type_vector as the distance between
 * the start of the 1st block and the end of the last block. This means that the
 * second vector begins only after the last block of the first vector. Although
 * this makes sense when vectors are made of contiguous data, in our case it
 * does not generate the repeating pattern we want:
 *
 * +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 * |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |
 * +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 * ^                                                           ^
 * |                                                           |
 * +-----------------------------------------------------------+
 *                 First vector effectively                    ^
 *             extracting all cells of column A                |
 *                                                             +---------... 
 *                                                              Second vector,
 *                                                         supposed to extract
 *                                                       all cells of column B
 *
 * Not only did the second vector missed some of the cells of column B, but
 * having started much further away in the buffer than it was meant to, it is
 * likely to result in a segmentation fault, similarly for C. In our case, we
 * want an vectors to have an interleaved pattern and begin after the first
 * block of the previous vector, as follows:
 * 
 * +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 * |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |
 * +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
 * ^     ^     ^
 * |     |     |
 * |     |     | Start of 3rd vector
 * |     |
 * |     | Start of 2nd vector
 * |      
 * | Start of 1st vector
 *
 * We therefore need to modify the vector extent accordingly and set it to the
 * size of 1 block, this is where we need the MPI_Type_create_resized routine.
 *
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

	// Number of cells per column.
	const int CELLS_PER_COLUMN = 4;
	// Number of per row.
	const int CELLS_PER_ROW = 3;

	// Create the vector datatype
	MPI_Datatype column_not_resized;
	MPI_Type_vector(CELLS_PER_COLUMN, 1, CELLS_PER_ROW, MPI_INT, &column_not_resized);

	// Resize it to make sure it is interleaved when repeated
	MPI_Datatype column_resized;
	MPI_Type_create_resized(column_not_resized, 0, sizeof(int), &column_resized);
	MPI_Type_commit(&column_resized);

	// Get my rank and do the corresponding job
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	int my_column[CELLS_PER_COLUMN];
	if(my_rank == 0)
	{
		// Declare and initialise the full array
		int full_array[CELLS_PER_COLUMN][CELLS_PER_ROW];
		for(int i = 0; i < CELLS_PER_COLUMN; i++)
		{
			for(int j = 0; j < CELLS_PER_ROW; j++)
			{
				full_array[i][j] = i * CELLS_PER_ROW + j;
			}
		}

		// Send the column
		MPI_Scatter(full_array, 1, column_resized, my_column, CELLS_PER_COLUMN, MPI_INT, 0, MPI_COMM_WORLD);
	}
	else
	{
		// Receive the column
		MPI_Scatter(NULL, 1, column_resized, my_column, CELLS_PER_COLUMN, MPI_INT, 0, MPI_COMM_WORLD);
	}

	printf("MPI process %d received column made of cells %d, %d, %d, %d\n", my_rank, my_column[0], my_column[1], my_column[2], my_column[3]);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
