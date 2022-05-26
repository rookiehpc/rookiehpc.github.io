#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to check the state of the dynamic adjustment.
 **/
int main(int argc, char* argv[])
{
	if(omp_get_dynamic())
	{
		printf("By default, dynamic adjustment is allowed.\n");
	}
	else
	{
		printf("By default, dynamic adjustment is not allowed.\n");
	}

	// Invert the dynamic adjustement
	omp_set_dynamic(omp_get_dynamic() ? 0 : 1);

	if(omp_get_dynamic())
	{
		printf("Dynamic adjustment is now allowed.\n");
	}
	else
	{
		printf("Dynamic adjustment is no longer allowed.\n");
	}

	return EXIT_SUCCESS;
}