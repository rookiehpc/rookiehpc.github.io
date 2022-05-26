#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to use an atomic construct.
 * @details This application consists of 4 threads incrementing a shared
 * variable. This situation is a classic data-race configuration, but the atomic
 * construct guarantees correctness by making sure the variable is accessed
 * atomically. In a nutshell, altough we still do not know in which order the
 * increments are done, we are certain they do not corrupt each other. This
 * situation is to illustrate the atomic construct, in real-world codes, you
 * will want to use a reduction clause instead.
 **/
int main(int argc, char* argv[])
{
	// Use 4 threads when creating OpenMP parallel regions
	omp_set_num_threads(4);

	int total = 0;

	// Create the parallel region
	#pragma omp parallel default(none) shared(total)
	{
		for(int i = 0; i < 10; i++)
		{
			// Atomically add one to the total
			#pragma omp atomic
			total++;
		}
	}

	printf("Total = %d.\n", total);

	return EXIT_SUCCESS;
}
