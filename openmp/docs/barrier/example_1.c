#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates the OpenMP barrier synchronisation.
 * @details This application is made of a parallel region, in which two distinct
 * parts are to be executed, separated with a barrier. In each part, threads
 * have to print a message. They will print their second message only when all
 * threads will have printed the first one.
 **/
int main(int argc, char* argv[])
{
	// Use 4 threads when we create a parallel region
	omp_set_num_threads(4);

	// Create the parallel region
	#pragma omp parallel
	{
		// Threads print their first message
		printf("[Thread %d] I print my first message.\n", omp_get_thread_num());

		// Make sure all threads have printed their first message before moving on.
		#pragma omp barrier

		// One thread indicates that the barrier is complete.
		#pragma omp single
		{
			printf("The barrier is complete, which means all threads have printed their first message.\n");
		}

		// Threads print their second message
		printf("[Thread %d] I print my second message.\n", omp_get_thread_num());
	}

	return EXIT_SUCCESS;
}
