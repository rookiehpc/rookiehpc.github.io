#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates the guided scheduling policy.
 * @details A for loop is parallelised across 2 threads using the guided
 * scheduling policy.
 **/
int main(int argc, char* argv[])
{
	// Use 2 threads when creating OpenMP parallel regions
	omp_set_num_threads(2);

	// Parallelise the for loop using the dynamic schedule
	#pragma omp parallel for schedule(guided)
	for(int i = 0; i < 10; i++)
	{
		printf("Thread %d processes iteration %d.\n", omp_get_thread_num(), i);
	}

	return EXIT_SUCCESS;
}
