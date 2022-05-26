!> @brief Illustrates the use the runtime schedule.
!> @details A for loop is parallelised across 4 threads using the runtime schedule policy.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: i

    ! Use 4 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(4)

    ! Parallelise the for loop using the runtime schedule
    !$omp parallel do schedule(runtime)
    DO i = 0, 9
        WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' processes iteration ', i, '.'
    END DO
    !$omp end parallel do
END PROGRAM main