!> @brief Illustrates the OpenMP shared policy.
!> @details This example shows that a variable passed as shared to a parallel
!> region is shared across all threads of that region. It becomes visible by
!> letting a thread modify its value, and another thread prints its value
!> afterwards. The whole process is made more visible by also printing the value
!> of the original variable before and after the OpenMP parallel region.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! The int that will be shared among threads
    INTEGER :: val = 0

    ! Use 2 OpenMP threads
    CALL omp_set_num_threads(2)

    WRITE(*,'(A,I0,A)') 'Value of "val" before the OpenMP parallel region: ', val, '.'

    !$omp parallel default(none) shared(val)
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

    WRITE(*,'(A,I0,A)') 'Value of "val" after the OpenMP parallel region: ', val, '.'
END PROGRAM main