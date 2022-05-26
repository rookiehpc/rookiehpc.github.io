#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to use the for construct.
 * @details This application contains a for loop that initialises an array. This
 * for loop is parallelised by using a for construct inside a parallel region.
 **/
int main(int argc, char* argv[])
{
	// Use 2 threads when creating OpenMP parallel regions
	omp_set_num_threads(2);

	int a[10];

	// Spawn the threads
	#pragma omp parallel
	{
		// Tell the threads to share these iterations rather than running the entire iteration set each
		#pragma omp for
		for(int i = 0; i < 10; i++)
		{
			a[i] = i;
		}
	}

	return EXIT_SUCCESS;
}
