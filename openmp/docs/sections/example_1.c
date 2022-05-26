#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to use a sections clause.
 * @details A parallel region is created, in which a sections worksharing
 * construct is built, containing multiple section clauses defining jobs to do
 * by threads.
 **/
int main(int argc, char* argv[])
{
	// Use 4 threads when creating OpenMP parallel regions
	omp_set_num_threads(4);

	// Create the parallel region
	#pragma omp parallel
	{
		// Create the sections
		#pragma omp sections
		{
			// Generate the first section
			#pragma omp section
			{
				printf("Section 1 is executed by thread %d.\n", omp_get_thread_num());
			}

			// Generate the second section
			#pragma omp section
			{
				printf("Section 2 is executed by thread %d.\n", omp_get_thread_num());
			}
		}
	}

	return EXIT_SUCCESS;
}
