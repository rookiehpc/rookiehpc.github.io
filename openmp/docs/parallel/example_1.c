#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates the OpenMP parallel region creation.
 * @details Two parallel regions are created:
 *     - 1st one: using the number of threads indicated by the OMP_NUM_THREADS environment variable
 *     - 2nd step: using the number of threads overriden by omp_set_num_threads
 **/
int main(int argc, char* argv[])
{
	#pragma omp parallel
	{
		#pragma omp single
		{
			printf("From the environment variable, parallel regions contain %d threads.\n", omp_get_num_threads());
		}
	}


	omp_set_num_threads(7);
	#pragma omp parallel
	{
		#pragma omp single
		{
			printf("After overwriting, parallel regions contain %d threads.\n", omp_get_num_threads());
		}
	}

	return EXIT_SUCCESS;
}
