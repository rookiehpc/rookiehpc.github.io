#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates the effect of the environment variable OMP_NUM_THREADS.
 * @details This application assumes that the environment variable
 * OMP_NUM_THREADS has been set to a specific number. For instance, to put the
 * value 4 in the environment variable OMP_NUM_THREADS:
 * - For Windows: SET OMP_NUM_THREADS=4
 * - For Linux & Mac: export OMP_NUM_THREADS=4
 * This application is meant to be executed after the environment variable
 * OMP_NUM_THREADS was updated. It creates a parallel region, and prints the
 * number of threads in that parallel region, which is that of OMP_NUM_THREADS.
 **/
int main(int argc, char* argv[])
{
	#pragma omp parallel
	{
		// Each thread prints its identifier
		printf("We are %d threads, I am thread %d.\n", omp_get_num_threads(), omp_get_thread_num());
	}

	return EXIT_SUCCESS;
}
