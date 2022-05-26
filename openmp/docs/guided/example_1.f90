!> @brief Illustrates the guided scheduling policy.
!> @details A for loop is parallelised across 2 threads using the guided
!> scheduling policy.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: i

    ! Use 2 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(2)

    ! Parallelise the for loop using the dynamic schedule
    !$omp parallel
        !$omp do schedule(guided)
        DO i = 0, 9
            WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num() ,' processes iteration ', i ,'.'
        END DO
        !$omp end do
    !$omp end parallel
END PROGRAM main