#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to get the number of threads.
 * @details This code prints the number of threads at two specific locations:
 *     1) Outside an OpenMP parallel region
 *     2) Inside an OpenMP parallel region
 **/
int main(int argc, char* argv[])
{
	// Tell OpenMP to use 4 threads in parallel regions
	omp_set_num_threads(4);

	// 1) Outside the OpenMP parallel region
	printf("Outside the OpenMP parallel region, we are %d threads.\n", omp_get_num_threads());

	// Create the OpenMP parallel region, which will contain 4 threads
	#pragma omp parallel
	{
		// 2) Inside the OpenMP parallel region
		printf("Inside the OpenMP parallel region, we are %d threads.\n", omp_get_num_threads());
	}

	return EXIT_SUCCESS;
}