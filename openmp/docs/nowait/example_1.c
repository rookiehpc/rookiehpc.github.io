#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <omp.h>

/**
 * @brief Illustrates how to use a nowait clause.
 * @details A parallel region is created, in which one thread executes a single
 * construct while the other one skips it without waiting thanks to the nowait
 * clause.
 **/
int main(int argc, char* argv[])
{
	// Use 4 threads when c{
    "Type": "Documentation",
    "Technology": "OpenMP",
    "Name": "critical",
    "DirectoryName": "critical",
    "SharedDescription": true,
    "Description": "The critical construct, which is used inside parallel regions, tells OpenMP that the associated block is to be executed by every thread but no more than one thread at a time. The critical construct must not be confused with the single or master constructs.",
    "Categories": [
        "Synchronisation"
    ],
    "Languages": [
        {
            "Language": "C",
            "Prototype":"#pragma omp critical [(name) [[,] hint(hint-expression)]] <new-line>\n    <structured-block>",
            "Parameters": [
                
            ]
        },
        {
            "Language": "FORTRAN-90",
            "Prototype":"!$omp critical [(name) [[,] hint(hint-expression)]]\n    <structured-block>\n!$omp end critical [(name)]",
            "Parameters": [
                
            ]
        }
    ]
}reating OpenMP parallel regions
	omp_set_num_threads(2);

	// This semaphore is used to sequentialise printfs.
	bool handshake = false;

	// Create the parallel region
	#pragma omp parallel default(none) shared(handshake)
	{
		#pragma omp single nowait
		{
			printf("Thread %d got into the single construct.\n", omp_get_thread_num());
			while(!handshake)
			{

			}
		}

		#pragma omp critical
		{
			if(!handshake)
			{
				printf("Thread %d skipped the single clause, not waiting for the other thread thanks to the nowait clause.\n", omp_get_thread_num());
				handshake = true;
			}
			else
			{
				printf("Thread %d has now completed the single construct.\n", omp_get_thread_num());
			}
		}
	}

	return EXIT_SUCCESS;
}
