#include <stdio.h>
#include <omp.h>

/**
 * @brief Solution to the Hello world exercise in OpenMP.
 **/
int main(int argc, char* argv[])
{
	// 1) Create the OpenMP parallel region
	#pragma omp parallel default(none)
	{
		// 1.1) Get my thread number
		int my_id = omp_get_thread_num();

		// 1.2) Get the number of threads inside that parallel region
		int thread_number = omp_get_num_threads();

		// 1.3) Print everything
		printf("\"Hello world!\" from thread %d, we are %d threads.\n", my_id, thread_number);
	}

	return 0;
}