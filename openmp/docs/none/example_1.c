#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates the OpenMP none policy.
 * @details This example shows that the usage of the "none" default, by
 * comparing a version using implicit data-sharing clauses against that using
 * explicit data-sharing clauses. Both yield the same result, but one requires
 * the explicit usage of data-sharing clauses.
 **/
int main(int argc, char* argv[])
{
	// Use 2 OpenMP threads
	omp_set_num_threads(2);

	// Relying on the implicit default(shared)
	int implicitlyShared = 0;
	#pragma omp parallel
	{
		#pragma omp atomic
		implicitlyShared++;
	}
	printf("Value with implicit shared: %d.\n", implicitlyShared);

	// Forcing the usage of explicit data-sharing closes
	int explicitlyShared = 0;
	#pragma omp parallel default(none) shared(explicitlyShared)
	{
		#pragma omp atomic
		explicitlyShared++;
	}
	printf("Value with explicit shared: %d.\n", explicitlyShared);

	return EXIT_SUCCESS;
}
