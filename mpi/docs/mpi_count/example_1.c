#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to use the MPI_Count datatype.
 * @details This application consists of 2 MPI processes. MPI process 0 creates
 * a count, and sends it to MPI process 1, which then prints it.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Check that 2 MPI processes are spawn
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(comm_size != 2)
	{
		printf("This application is meant to be run with 2 MPI processes, not %d.\n", comm_size);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	enum roles {SENDER, RECEIVER};

	switch(my_rank)
	{
		case SENDER:
		{
			// Create the MPI datatype
			MPI_Datatype my_type;
			MPI_Type_contiguous(10, MPI_INT, &my_type);
			MPI_Type_commit(&my_type);

			// Retrieve the size of the MPI datatype created
			MPI_Count count;
			MPI_Type_size_x(my_type, &count);
			MPI_Send(&count, 1, MPI_COUNT, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			MPI_Count count;
			MPI_Recv(&count, 1, MPI_COUNT, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("The type created would generate a message of %lld bytes.\n", count);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
