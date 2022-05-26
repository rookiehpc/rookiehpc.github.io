#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to tell OpenMP which schedule to apply.
 * @details A static schedule strategy is explicitly specified, as well as the chunksize.
 **/
int main(int argc, char* argv[])
{
	// Use 2 threads when creating OpenMP parallel regions
	omp_set_num_threads(2);

	// Parallelise the for loop using the static schedule with chunks made of 2 iterations
	#pragma omp parallel for schedule(static, 2)
	for(int i = 0; i < 10; i++)
	{
		printf("Thread %d processes iteration %d.\n", omp_get_thread_num(), i);
	}

	return EXIT_SUCCESS;
}
