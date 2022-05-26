#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates the use of the omp_sched_t.
 * @details A for loop is parallelised across 4 threads using the runtime
 * schedule policy. The schedule to apply is defined by omp_set_schedule as 
 * being a dynamic scheduling with chunks made of 4 iterations.
 **/
int main(int argc, char* argv[])
{
	// Use 4 threads when creating OpenMP parallel regions
	omp_set_num_threads(4);

	// Tell OpenMP which scheduling policy and chunk size use for runtime schedule clauses
	omp_sched_t kind = omp_sched_dynamic;
	omp_set_schedule(kind, 2);

	// Parallelise the for loop using the runtime schedule
	#pragma omp parallel for schedule(runtime)
	for(int i = 0; i < 10; i++)
	{
		printf("Thread %d processes iteration %d.\n", omp_get_thread_num(), i);
	}

	return EXIT_SUCCESS;
}
