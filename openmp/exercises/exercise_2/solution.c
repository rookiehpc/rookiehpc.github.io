#include <stdio.h>
#include <omp.h>

#define ARRAY_SIZE 10

int main(int argc, char* argv[])
{
	int a[ARRAY_SIZE];

	// 1) Create the OpenMP parallel region
	#pragma omp parallel
	{
		// 1.1) Create the for construct and initialise the array elements
		#pragma omp for
		for(int i = 0; i < ARRAY_SIZE; i++)
		{
			a[i] = i * i;
		}
	}

	// 2) Print the array elements, sequentially
	for(int i = 0; i < ARRAY_SIZE; i++)
	{
		printf("a[%d] = %2d\n", i, a[i]);
	}

	return 0;
}