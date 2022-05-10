#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to obtain the address of an element.
 **/
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	int a[10];

	MPI_Aint addr;
	MPI_Get_address(&a[2], &addr);

	printf("The address of the third element is %ld.\n", addr);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
