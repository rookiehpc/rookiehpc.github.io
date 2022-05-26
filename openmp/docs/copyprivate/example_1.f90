!> @brief Illustrates how to use the copyprivate clause.
!> @details This application passes a variable as firstprivate to a parallel
!> region. Then, a single construct receives this variable as a copyprivate and
!> modifies its values. All threads print the value of their own copy before and
!> after the single construct. Although each thread has its own copy, the
!> copyprivate will have broadcasted the new value to all threads after the 
!> single construct.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: a = 123

    !$omp parallel default(none) firstprivate(a)
        WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ': a = ', a, '.'

        !$omp barrier

        !$omp single
            a = 456
            WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' executes the single construct and changes a to ', a, '.'
        !$omp end single copyprivate(a)

        WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ': a = ', a, '.'
    !$omp end parallel
END PROGRAM main