#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates the static scheduling distribution.
 * @details A for loop is parallelised across 2 threads using the static policy, in 2 situations:
 *     1) No chunksize passed
 *         - Thread 0: first half of the iterations
 *         - Thread 1: second half of the iterations
 *     2) A chunksize of 2 iterations
 *         - Thread 0 takes the first chunk, the third chunk, the fifth chunk and so on...
 *         - Thread 1 takes the second chunk, the fourth chunk, the sixth chunk and so on...
 **/
int main(int argc, char* argv[])
{
	// Use 2 threads when creating OpenMP parallel regions
	omp_set_num_threads(2);

	printf("With no chunksize passed:\n");

	// Parallelise the for loop using the static schedule
	#pragma omp parallel for schedule(static)
	for(int i = 0; i < 10; i++)
	{
		printf("Thread %d processes iteration %d.\n", omp_get_thread_num(), i);
	}

	printf("With a chunksize of 2:\n");

	// Parallelise the for loop using the static schedule and chunks of 2 iterations
	#pragma omp parallel for schedule(static, 2)
	for(int i = 0; i < 10; i++)
	{
		printf("Thread %d processes iteration %d.\n", omp_get_thread_num(), i);
	}

	return EXIT_SUCCESS;
}
