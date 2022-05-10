#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <tgmath.h>
#include <mpi.h>

/**
 * @brief Illustrate how to communicate an array of long double precision floats
 * between 2 MPI processes.
 * @details This application is meant to be run with 2 MPI processes: 1 sender
 * and 1 receiver. The former sends an array of long double precision floats to
 * the latter, which prints it.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Check that 2 MPI processes are used.
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 2)
	{
		printf("This application is meant to be run with 2 MPI processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Get my rank and do the corresponding job.
	enum role_ranks { SENDER, RECEIVER };
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	switch(my_rank)
	{
		case SENDER:
		{
			// Send the long double complexes
			long double _Complex complexesToSend[2] = { I * I, 2 * I * I };
			printf("[MPI process %d] I send long double complexes: %.1Lf+%.1Lfi and %.1Lf+%.1Lfi.\n", my_rank, creall(complexesToSend[0]), cimagl(complexesToSend[0]), creall(complexesToSend[1]), cimagl(complexesToSend[1]));
			MPI_Ssend(complexesToSend, 2, MPI_C_LONG_DOUBLE_COMPLEX, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			// Receive the long double complexes
			long double _Complex complexesReceived[2];
			MPI_Recv(complexesReceived, 2, MPI_C_LONG_DOUBLE_COMPLEX, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("[MPI process %d] I received long double complexes: %.1Lf+%.1Lfi and %.1Lf+%.1Lfi.\n", my_rank, creall(complexesReceived[0]), cimagl(complexesReceived[0]), creall(complexesReceived[1]), cimagl(complexesReceived[1]));
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
