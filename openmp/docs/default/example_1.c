#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
 
/**
 * @brief Illustrates the OpenMP default clause.
 * @details An int is passed to a parallel region. Then:
 *     - 1st step: thread 0 writes "123" in the int
 *     - 2nd step: thread 1 prints the value of the int
 * The default policy, set to shared, becomes visible when the value read by thread 1 is the one written by thread 0.
 **/
int main(int argc, char* argv[])
{
    // Use 2 OpenMP threads
    omp_set_num_threads(2);
 
    // The int that will be shared among threads
    int val = 0;
 
 	// Variables not part of a data-sharing clause will be "shared" by default.
    #pragma omp parallel default(shared)
    {
        // Step 1: thread 0 writes the value
        if(omp_get_thread_num() == 0)
        {
            printf("Thread 0 sets the value of \"val\" to 123.\n");
            val = 123;
        }
 
        // Threads wait each other before progressing to step 2
        #pragma omp barrier
        
        // Step 2: thread 1 reads the value
        if(omp_get_thread_num() == 1)
        {
            printf("Thread 1 reads the value of \"val\": %d.\n", val);
        }
    }
 
    return EXIT_SUCCESS;
}

