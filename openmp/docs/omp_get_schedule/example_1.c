#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates the use the omp_get_schedule function.
 * @details The application retrieves the schedule and chunk size applied when
 * a runtime schedule is encountered.
 **/
int main(int argc, char* argv[])
{
	// Get the schedule to apply when a runtime schedule is encountered
	omp_sched_t kind;
	int chunk_size;
	omp_get_schedule(&kind, &chunk_size);

	printf("In case a runtime schedule is encountered, the ");
	switch(kind)
	{
		case omp_sched_static:
			printf("static");
			break;
		case omp_sched_dynamic:
			printf("dynamic");
			break;
		case omp_sched_guided:
			printf("guided");
			break;
		case omp_sched_auto:
			printf("auto");
			break;
		default:
			printf("other (implementation specific)");
			break;
	}
	printf(" schedule is applied, with chunks made of %d iteration%s.\n", chunk_size, (chunk_size > 1) ? "s" : "");

	return EXIT_SUCCESS;
}
