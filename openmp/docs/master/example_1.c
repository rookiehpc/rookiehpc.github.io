#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to use a master clause.
 * @details A parallel region is created, in which a certain part is executed by
 * every thread, and another part is executed only by the master thread.
 **/
int main(int argc, char* argv[])
{
	// Use 4 threads when creating OpenMP parallel regions
	omp_set_num_threads(4);

	// Create the parallel region
	#pragma omp parallel
	{
		printf("[Thread %d] Every thread executes this printf.\n", omp_get_thread_num());

		#pragma omp barrier

		#pragma omp master
		{
			printf("[Thread %d] Only the master thread executes this printf, which is me.\n", omp_get_thread_num());
		}
	}

	return EXIT_SUCCESS;
}
