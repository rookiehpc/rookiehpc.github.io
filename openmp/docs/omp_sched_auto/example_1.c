#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates the use of omp_sched_auto.
 * @details A for loop is parallelised across 4 threads using the runtime
 * schedule policy. The schedule to apply is defined by omp_set_schedule as 
 * being an auto scheduling. The chunk size input is required but ignored so any
 * value is acceptable.
 **/
int main(int argc, char* argv[])
{
	// Use 4 threads when creating OpenMP parallel regions
	omp_set_num_threads(4);

	// Tell OpenMP which scheduling policy and chunk size use for runtime schedule clauses
	omp_set_schedule(omp_sched_auto, 0);

	// Parallelise the for loop using the runtime schedule
	#pragma omp parallel for schedule(runtime)
	for(int i = 0; i < 10; i++)
	{
		printf("Thread %d processes iteration %d.\n", omp_get_thread_num(), i);
	}

	return EXIT_SUCCESS;
}
