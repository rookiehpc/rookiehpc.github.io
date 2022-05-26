#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates the OpenMP linear policy.
 * @details This example shows that when a linear variable is passed to a
 * parallelised for loop, the value of that variable is the original value plus 
 * the iteration logical number times the linear-step. After the OpenMP parallel
 * for, the value of the original variable is that of the linear variable at the
 * last iteration.
 **/
int main(int argc, char* argv[])
{
	// Use 4 OpenMP threads
	omp_set_num_threads(4);

	// Variable that will be private
	int val = 1;

	printf("Value of \"val\" before the OpenMP parallel for is %d.\n", val);

	#pragma omp parallel for linear(val:2)
	for(int i = 0; i < 10; i++)
	{
		printf("Thread %d sees \"val\" = %d at iteration %d.\n", omp_get_thread_num(), val, i);
	}

	printf("Value of \"val\" after the OpenMP parallel for is %d.\n", val);

	return EXIT_SUCCESS;
}
