#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to open a file via MPI.
 * @details This application illustrates how to open  a temporary file for the
 * duration of the MPI execution.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	MPI_File handle;
	int access_mode = MPI_MODE_CREATE /* Create the file if it does not exist */
					| MPI_MODE_EXCL /* The file must not exist, to avoid mistakenly erasing a file */
	                | MPI_MODE_RDWR /* With read / write access */
	                | MPI_MODE_UNIQUE_OPEN /* The file will not be opened concurrently elsewhere */
	                | MPI_MODE_DELETE_ON_CLOSE; /* Delete the file when it is closed */
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