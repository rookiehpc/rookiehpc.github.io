#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to tell OpenMP to apply the schedule specified from
 * OMP_SCHEDULE.
 * @details The runtime schedule is specified so that OpenMP fetches the
 * scheduling policy to apply from the environment variable OMP_SCHEDULE. In
 * this example, we assume that OMP_SCHEDULE has been set to "static,4".
 **/
int main(int argc, char* argv[])
{
	// Use 2 threads when creating OpenMP parallel regions
	omp_set_num_threads(2);

	// Parallelise the for loop using the runtime schedule, thus applying what OMP_SCHEDULE says
	#pragma omp parallel for schedule(runtime)
	for(int i = 0; i < 10; i++)
	{
		printf("Thread %d processes iteration %d.\n", omp_get_thread_num(), i);
	}

	return EXIT_SUCCESS;
}
