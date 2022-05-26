#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to use the num_threads clause.
 * @details This code generates three parallel regions. The number of threads to
 * use for all parallel regions has been set to 4 by the omp_set_num_threads
 * function. This value will be overwritten for the second parallel region only,
 * using the num_threads clause.
 **/
int main(int argc, char* argv[])
{
	// Tell OpenMP to use 4 threads in parallel regions from now on
	omp_set_num_threads(4);

	// Create a first parallel region, which therefore contains 4 threads
	#pragma omp parallel
	{
		// Each thread prints its identifier
		printf("Loop 1: We are %d threads, I am thread %d.\n", omp_get_num_threads(), omp_get_thread_num());
	}

	// Create a second parallel region, overwriting the number of threads to use with num_threads
	#pragma omp parallel num_threads(3)
	{
		// Each thread prints its identifier
		printf("Loop 2: We are %d threads, I am thread %d.\n", omp_get_num_threads(), omp_get_thread_num());
	}

	// Create a third parallel region, no overwriting from num_threads, so back to 4 threads again
	#pragma omp parallel
	{
		// Each thread prints its identifier
		printf("Loop 3: We are %d threads, I am thread %d.\n", omp_get_num_threads(), omp_get_thread_num());
	}

	return EXIT_SUCCESS;
}