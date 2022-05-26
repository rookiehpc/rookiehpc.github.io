#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to measure time using walltime.
 * @details This code waits for a number of seconds given, by having a loop that
 * checks how many seconds elapsed since it started, and keeps looping until
 * enough seconds have passed by.
 **/
int main(int argc, char* argv[])
{
	// Time before the loop began
	double start_time;
	// Time right now
	double current_time;
	// Elapsed time since beginning of loop
	double elapsed_time;
	// Number of seconds to wait
	double to_wait = 3;

	// Initialise the start time
	start_time = omp_get_wtime();
	// Initialise the end time
	current_time = omp_get_wtime();
	// Initialise the elapsed time
	elapsed_time = current_time - start_time;

	// As long as fewer than "to_wait" seconds elapsed, keep looping
	while(elapsed_time < to_wait)
	{
		// Get the new time
		current_time = omp_get_wtime();
		// Update the elapsed time
		elapsed_time = current_time - start_time;
	}

	printf("%f seconds have elapsed.\n", elapsed_time);

	return EXIT_SUCCESS;
}