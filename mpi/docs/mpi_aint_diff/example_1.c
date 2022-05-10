#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to get the difference between addresses.
 * @details This application takes the address of two elements in an array and
 * calculates the difference between the two before printing the difference.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	int a[10];

	MPI_Aint addr_1;
	MPI_Get_address(&a[2], &addr_1);

	MPI_Aint addr_2;
	MPI_Get_address(&a[8], &addr_2);

	MPI_Aint addr_gap;
	addr_gap = MPI_Aint_diff(addr_2, addr_1);

	printf("Difference between the address of the 3rd int and 9th int is %ld bytes.\n", addr_gap);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
