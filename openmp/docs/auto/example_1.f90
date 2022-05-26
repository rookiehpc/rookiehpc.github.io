!> @brief Illustrates the use the auto schedule.
!> @details A for loop is parallelised across 2 threads using the auto schedule policy.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: i

    ! Use 2 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(4)

    ! Parallelise the for loop using the auto schedule
    !$omp parallel
        !$omp do schedule(auto)
        DO i = 0, 9
            WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' processes iteration ', i, '.'
        END DO
        !$omp end do
    !$omp end parallel
END PROGRAM main