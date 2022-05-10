#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to ensure that a file is created during an MPI file
 * opening, with MPI_MODE_EXCL.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	MPI_File handle;
	int access_mode = MPI_MODE_CREATE /* Create the file if it does not exist */
				    | MPI_MODE_EXCL /* The file must not already exist */
					| MPI_MODE_RDWR; /* With read-write access */
	if(MPI_File_open(MPI_COMM_WORLD, "file.tmp", access_mode, MPI_INFO_NULL, &handle) != MPI_SUCCESS)
	{
		printf("[MPI process %d] Failure in opening the file.\n", my_rank);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	printf("[MPI process %d] File opened successfully.\n", my_rank);

	if(MPI_File_close(&handle) != MPI_SUCCESS)
	{
		printf("[MPI process %d] Failure in closing the file.\n", my_rank);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	printf("[MPI process %d] File closed successfully.\n", my_rank);

	MPI_Finalize();

	return EXIT_SUCCESS;
}