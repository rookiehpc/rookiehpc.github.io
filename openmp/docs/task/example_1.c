#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to create tasks.
 * @details This application consists of a thread, in an OpenMP parallel
 * region, that spawns tasks.
 **/
int main(int argc, char* argv[])
{
	// Use 3 threads when creating OpenMP parallel regions
	omp_set_num_threads(3);

	// Create the parallel region
	#pragma omp parallel
	{
		// One thread will spawn the tasks
		#pragma omp single
		{
			// Spawn the first task
			#pragma omp task
			{
				printf("Task 1 executed by thread %d.\n", omp_get_thread_num());
			}

			// Spawn the second task
			#pragma omp task
			{
				printf("Task 2 executed by thread %d.\n", omp_get_thread_num());
			}

			// Wait for both tasks to finish
			#pragma omp taskwait
		}
	}

	return EXIT_SUCCESS;
}
