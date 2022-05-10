#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to get the name of a communicator.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get the name of the default communicator
	char name[MPI_MAX_OBJECT_NAME];
	int name_length;
	MPI_Comm_get_name(MPI_COMM_WORLD, name, &name_length);
	printf("The default communicator is named \"%s\".\n", name);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
