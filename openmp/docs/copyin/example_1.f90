 !> @brief Illustrates how to use the copyin clause.
 !> @details This application declares a global variable and specifies it as
 !> threadprivate. This variable is then passed a copyin to the first parallel
 !> region. In that region, the master thread modifies its value but other
 !> threads will not see the update until the second parallel region; where the
 !> variable will be passed as copyin again.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: a = 12345
    COMMON /C1/ a

    !$omp threadprivate(/C1/)

    ! Turn off dynamic threads as required by threadprivate
    CALL omp_set_dynamic(.FALSE.)

    !$omp parallel copyin(a)
        !$omp master
            WRITE(*,'(A)') '(First parallel region) Master thread changes the value of a to 67890.'
            a = 67890
        !$omp end master

        !$omp barrier

        WRITE(*,'(A,I0,A,I0,A)') '(First parallel region) Thread ', omp_get_thread_num(), ': a = ', a, '.'
    !$omp end parallel

    !$omp parallel copyin(a)
        WRITE(*,'(A,I0,A,I0,A)') '(Second parallel region) Thread ', omp_get_thread_num(), ': a = ', a, '.'
    !$omp end parallel
END PROGRAM main