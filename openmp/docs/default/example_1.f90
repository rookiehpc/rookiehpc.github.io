!> @brief Illustrates the OpenMP default clause.
!> @details An int is passed to a parallel region. Then:
!>     - 1st step: thread 0 writes '123' in the int
!>     - 2nd step: thread 1 prints the value of the int
!> The default policy, set to shared, becomes visible when the value read by thread 1 is the one written by thread 0.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! The int that will be shared among threads
    INTEGER :: val = 0

    ! Use 2 OpenMP threads
    CALL omp_set_num_threads(2)
 
     ! Variables not part of a data-sharing clause will be 'shared' by default.
    !$omp parallel default(shared)        
        ! Step 1: thread 0 writes the value
        IF (omp_get_thread_num() .EQ. 0) THEN
            WRITE(*,'(A)') 'Thread 0 sets the value of "val" to 123.'
            val = 123
        END IF
 
        ! Threads wait each other before progressing to step 2
        !$omp barrier
        
        ! Step 2: thread 1 reads the value
        IF (omp_get_thread_num() .EQ. 1) THEN
            WRITE(*,'(A,I0,A)') 'Thread 1 reads the value of "val": ', val, '.'
        END IF
    !$omp end parallel
END PROGRAM main

