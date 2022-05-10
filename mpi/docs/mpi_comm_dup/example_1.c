#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to duplicate a communicator with MPI_Comm_dup.
 * @details This application is meant to run with 2 MPI processes, one acting as
 * the sender in the use-case, one acting as the receiver. This application
 * illustrates how to duplicate a communicator, and also provides a use-case
 * for MPI_Comm_dup.
 * 
 * To that end, we imagine an user code that would:
 *     1.1) send an MPI_INT from MPI process 0 to MPI process 1 with tag 0
 *     1.2) issue the corresponding MPI_Recv on MPI process 1 with tag 0
 * 
 * We also imagine an MPI library running in parallel in the background, which
 * would:
 *     2.1) send an MPI_INT from MPI process 0 to MPI process 1 with tag 0
 *     2.2) issue the corresponding MPI_Recv on MPI process 1 with tag 0
 * 
 * Both sends and receives would thus be identical; same sender rank, same
 * receiver rank, same element datatype, same element count, same tag and same
 * communicator. Therefore, the MPI_Recv of the MPI library could very well be
 * mismatched with the MPI_Send from the user code in the event of an
 * unfortunate timing.
 *
 * To avoid this risk, a duplicated communicator will be passed to that library.
 * It results that, albeit having a communicator identical to the original one,
 * the MPI calls issued by the MPI library will be on a different communicator,
 * hence cannot be mismatched with those that the user issued on the original
 * communicator.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get my rank in the MPI_COMM_WORLD communicator
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Duplicate the MPI_COMM_WORLD communicator; everything is preserved, ranks included
	MPI_Comm duplicated_communicator;
	MPI_Comm_dup(MPI_COMM_WORLD, &duplicated_communicator);

	// The actual communicator duplication with MPI_Comm_dup is complete by now.
	// Below is a use case illustrating the usefulness of MPI_Comm_dup.

	// An MPI_Send issued from the user code
	if(my_rank == 0)
	{
		int some_message_in_user_code = 1234;
		MPI_Send(&some_message_in_user_code, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
		printf("[MPI process 0] Sent %d in user code.\n", some_message_in_user_code);
	}

	// An MPI_Send issued by an MPI library running in parallel
	if(my_rank == 0)
	{
		int some_message_in_library_code = 5678;
		MPI_Send(&some_message_in_library_code, 1, MPI_INT, 1, 0, duplicated_communicator);
		printf("[MPI process 0] Sent %d in library code.\n", some_message_in_library_code);
	}

	// Same emitter rank, same receiver rank, same element datatype, same
	// element count but different communicators, they cannot be mismatched.

	// The user code can safely issue its MPI_Recv
	if(my_rank == 1)
	{
		int buffer_for_user_message;
		MPI_Recv(&buffer_for_user_message, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		printf("[MPI process 1] Received %d in user code.\n", buffer_for_user_message);
	}

	// The MPI library can safely issue its MPI_Recv
	if(my_rank == 1)
	{
		int buffer_for_library_message;
		MPI_Recv(&buffer_for_library_message, 1, MPI_INT, 0, 0, duplicated_communicator, MPI_STATUS_IGNORE);
		printf("[MPI process 1] Received %d in library code.\n", buffer_for_library_message);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
