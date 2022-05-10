#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to set the name of a communicator.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Set the name of the default communicator
	char name[] = "My default communicator";
	MPI_Comm_set_name(MPI_COMM_WORLD, name);

	// Get the name of the default communicator and print it
	char verification_name[MPI_MAX_OBJECT_NAME];
	int verification_name_length;
	MPI_Comm_get_name(MPI_COMM_WORLD, verification_name, &verification_name_length);
	printf("The default communicator has been renamed as \"%s\".\n", verification_name);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
