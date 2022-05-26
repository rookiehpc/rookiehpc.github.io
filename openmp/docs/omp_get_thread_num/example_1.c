#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to get a thread identifier.
 * @details This code generates a parallel region, in which threads print their
 * identifier.
 **/
int main(int argc, char* argv[])
{
	// Tell OpenMP to use 4 threads in parallel regions
	omp_set_num_threads(4);

	// Create the OpenMP parallel region, which will contain 4 threads
	#pragma omp parallel
	{
		// Each thread prints its identifier
		printf("I am thread %d.\n", omp_get_thread_num());
	}

	return EXIT_SUCCESS;
}