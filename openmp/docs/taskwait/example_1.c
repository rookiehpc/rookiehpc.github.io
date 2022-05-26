#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Illustrates how to wait for the completion of child tasks.
 * @details This application consists of a thread, in an OpenMP parallel region,
 * that spawns tasks. It first spawns two tasks, then wait for these to complete
 * before spawning a third task. The execution flow can be visualised below:
 *
 * single construct
 *   |
 *   +------------------------------------------> spawns task 1 --------+
 *   |                                                                  |
 *   |                                                            +-----+-----+
 *   +------------------------------------------> spawns task 2   | A thread  |
 *   |                                                  |         | will pick |
 *   |                                            +-----+-----+   | it up and |
 *   |                                            | A thread  |   | execute   |
 *   |                                            | will pick |   | this task |
 *   |                                            | it up and |   +-----+-----+
 *   |                                            | execute   |         |
 *   |                                            | this task |         |
 *   |                                            +-----+-----+         |
 *   |                                                  |               |
 *   +--> waits for tasks 1 and 2 to complete      |////////////////////////|
 *   |
 *   +--> spawns task 3 ----------------------------------------------+
 *   |                                                                |
 *   |                                                          +-----+-----+
 *   |                                                          | A thread  |
 *   |                                                          | will pick |
 *   |                                                          | it up and |
 *   |                                                          | execute   |
 *   |                                                          | this task |
 *   |                                                          +-----+-----+
 *   |                                                                |
 *   +--> implicit barrier at the end of the single construct |///////////////|
 **/
int main(int argc, char* argv[])
{
    // Use 3 threads when creating OpenMP parallel regions
    omp_set_num_threads(3);

    // Create the parallel region
    #pragma omp parallel
    {
        #pragma omp single
        {
            // Spawn the first task
            #pragma omp task
            {
                printf("Task 1 executed by thread %d.\n", omp_get_thread_num());
            }

            // Spawn the second task
            #pragma omp task
            {
                printf("Task 2 executed by thread %d.\n", omp_get_thread_num());
            }

            // Wait for the two tasks above to complete before moving to the third one
            #pragma omp taskwait

            // One thread indicates that the synchronisation is finished
            printf("The taskwait construct completed, which means tasks 1 and 2 are complete. We can now move to task 3.\n");

            // Spawn the third task
            #pragma omp task
            {
                printf("Task 3 executed by thread %d.\n", omp_get_thread_num());
            }

            // The implicit barrier at the end of the single construct will wait for tasks to finish
        }
    }

    return EXIT_SUCCESS;
}
