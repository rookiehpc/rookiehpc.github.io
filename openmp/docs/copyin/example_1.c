#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

int a = 12345;
#pragma omp threadprivate(a)

/**
 * @brief Illustrates how to use the copyin clause.
 * @details This application declares a global variable and specifies it as
 * threadprivate. This variable is then passed a copyin to the first parallel
 * region. In that region, the master thread modifies its value but other
 * threads will not see the update until the second parallel region; where the
 * variable will be passed as copyin again.
 **/
int main(int argc, char* argv[])
{
	// Turn off dynamic threads as required by threadprivate
	omp_set_dynamic(0);

	#pragma omp parallel copyin(a)
	{
		#pragma omp master
		{
			printf("[First parallel region] Master thread changes the value of a to 67890.\n");
			a = 67890;
		}

		#pragma omp barrier

		printf("[First parallel region] Thread %d: a = %d.\n", omp_get_thread_num(), a);
	}

	#pragma omp parallel copyin(a)
	{
		printf("[Second parallel region] Thread %d: a = %d.\n", omp_get_thread_num(), a);
	}

	return EXIT_SUCCESS;
}