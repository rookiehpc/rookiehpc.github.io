#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to do a classic reduction.
 * @details This example consists in calculating the sum of all elements of an
 * array.
 **/
int main(int argc, char* argv[])
{
	// Use 2 threads when creating OpenMP parallel regions
	omp_set_num_threads(2);

	int total = 0;
	const int ARRAY_SIZE = 10;
	int* myArray = malloc(sizeof(int) * ARRAY_SIZE);
	if(myArray == NULL)
	{
		printf("Cannot allocate the array \"myArray\".\n");
		return EXIT_FAILURE;
	}

	// Initialise the array
	for(int i = 0; i < ARRAY_SIZE; i++)
	{
		myArray[i] = i;
	}

	// Calculate the sum of all elements
	#pragma omp parallel for default(none) shared(myArray) firstprivate(ARRAY_SIZE) reduction(+: total)
	for(int i = 0; i < ARRAY_SIZE; i++)
	{
		total += myArray[i];
	}

	printf("The sum of all array elements is equal to %d.\n", total);
	free(myArray);

	return EXIT_SUCCESS;
}
