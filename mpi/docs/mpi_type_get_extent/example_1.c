#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to retreive the lower bound and extent of an MPI
 * datatype.
 * @details Each process in this program creates a contiguous MPI_Datatype made 
 * of two MPI_INT, and then prints its lower bound and extent. This can be
 * visually described as follows:
 *
 * lower bound = 0 bytes
 * |
 * V
 * +---------+---------+
 * | MPI_INT | MPI_INT |
 * +---------+---------+
 *  <----------------->
 *        extent
 * 
 * (assuming 4-byte integers,
 * this is an extent of 8 bytes)
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get my rank
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Create the datatype
	MPI_Datatype double_int_type;
	MPI_Type_contiguous(2, MPI_INT, &double_int_type);
	MPI_Type_commit(&double_int_type);

	// Get the lower bound and extent of the datatype created
	MPI_Aint lower_bound;
	MPI_Aint extent;
	MPI_Type_get_extent(double_int_type, &lower_bound, &extent);
	printf("[MPI Process %d] Lower-bound = %lu bytes, extent = %lu bytes.\n", my_rank, lower_bound, extent);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
