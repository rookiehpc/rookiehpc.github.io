#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <omp.h>

/**
 * @brief Illustrates how to use a nowait clause.
 * @details A parallel region is created, in which one thread executes a single
 * construct while the other one skips it without waiting thanks to the nowait
 * clause.
 **/
int main(int argc, char* argv[])
{
	// Use 4 threads when creating OpenMP parallel regions
	omp_set_num_threads(2);

	// This semaphore is used to sequentialise printfs.
	bool handshake = false;

	// Create the parallel region
	#pragma omp parallel default(none) shared(handshake)
	{
		#pragma omp single nowait
		{
			printf("Thread %d got into the single construct.\n", omp_get_thread_num());
			while(!handshake)
			{

			}
		}

		#pragma omp critical
		{
			if(!handshake)
			{
				printf("Thread %d skipped the single clause, not waiting for the other thread thanks to the nowait clause.\n", omp_get_thread_num());
				handshake = true;
			}
			else
			{
				printf("Thread %d has now completed the single construct.\n", omp_get_thread_num());
			}
		}
	}

	return EXIT_SUCCESS;
}
