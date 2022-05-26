#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to use the copyprivate clause.
 * @details This application passes a variable as firstprivate to a parallel
 * region. Then, a single construct receives this variable as a copyprivate and
 * modifies its values. All threads print the value of their own copy before and
 * after the single construct. Although each thread has its own copy, the
 * copyprivate will have broadcasted the new value to all threads after the 
 * single construct.
 **/
int main(int argc, char* argv[])
{
	int a = 123;

	#pragma omp parallel default(none) firstprivate(a)
	{
		printf("Thread %d: a = %d.\n", omp_get_thread_num(), a);

		#pragma omp barrier

		#pragma omp single copyprivate(a)
		{
			a = 456;
			printf("Thread %d executes the single construct and changes a to %d.\n", omp_get_thread_num(), a);
		}

		printf("Thread %d: a = %d.\n", omp_get_thread_num(), a);
	}

	return EXIT_SUCCESS;
}