#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to obtain the MPI_COMM_PARENT name.
 * @details In order to get the MPI_COMM_PARENT name, an MPI process must have
 * been spawned by another MPI process. This application therefore does so, and
 * the MPI process spawned prints the name of its parent communicator which is
 * MPI_COMM_PARENT by default.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	MPI_Comm parent;
	MPI_Comm_get_parent(&parent);
	if(parent == MPI_COMM_NULL)
	{
		// We have no parent commuicator so we have been spawned directly by the user
		MPI_Comm child;
		int spawn_error;
		printf("We are processes spawned directly by you, we now spawn a new instance of an MPI application.\n");
		MPI_Comm_spawn(argv[0], MPI_ARGV_NULL, 1, MPI_INFO_NULL, 0, MPI_COMM_WORLD, &child, &spawn_error);
	}
	else
	{
		// We have been spawned by another MPI process so we do have a parent communicator
		char name[MPI_MAX_OBJECT_NAME];
		int name_length;
		MPI_Comm_get_name(parent, name, &name_length);
		printf("I have been spawned by MPI processes, my parent communicator is named \"%s\".\n", name);
	}	

	MPI_Finalize();

	return EXIT_SUCCESS;
}
