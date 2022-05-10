#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to find the parent communicator of an MPI process.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	MPI_Comm parent;
	MPI_Comm_get_parent(&parent);
	if(parent == MPI_COMM_NULL)
	{
		// We have no parent communicator so we have been spawned directly by the user
		MPI_Comm child;
		int spawn_error;
		printf("We are processes spawned directly by you, we now spawn a new instance of an MPI application.\n");
		MPI_Comm_spawn(argv[0], MPI_ARGV_NULL, 1, MPI_INFO_NULL, 0, MPI_COMM_WORLD, &child, &spawn_error);
	}
	else
	{
		// We have been spawned by another MPI process
		printf("I have been spawned by MPI processes.\n");
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
