#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to use the omp_set_num_threads function.
 * @details This code generates two parallel regions. The first one will use the
 * number of threads contained in the environment variable OMP_NUM_THREADS. The
 * code then overwrites that value for all upcoming parallel regions by calling
 * the omp_set_num_threads function and using 1 more thread than what was used
 * for the first parallel region. Finally, it creates a second parallel region
 * to show the difference in the number of threads.
 **/
int main(int argc, char* argv[])
{
	int current_num_threads = 0;

	// Create the OpenMP parallel region, containing the number of threads as defined by OMP_NUM_THREADS
	#pragma omp parallel
	{
		// Each thread prints its identifier
		printf("Loop 1: We are %d threads, I am thread %d.\n", omp_get_num_threads(), omp_get_thread_num());

		#pragma single
		{
			current_num_threads = omp_get_num_threads();
		}
	}

	! Tell OpenMP to now use one more thread in parallel regions
	omp_set_num_threads(current_num_threads+1);

	// Create the OpenMP parallel region, which will contain 8 threads
	#pragma omp parallel
	{
		// Each thread prints its identifier
		printf("Loop 1: We are %d threads, I am thread %d.\n", omp_get_num_threads(), omp_get_thread_num());
	}

	return EXIT_SUCCESS;
}