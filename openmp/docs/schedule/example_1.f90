!> @brief Illustrates how to tell OpenMP which schedule to apply.
!> @details A static schedule strategy is explicitly specified, as well as the chunksize.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: i

    ! Use 2 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(2)

    ! Parallelise the for loop using the static schedule with chunks made of 2 iterations
    !$omp parallel do schedule(static, 2)
    DO i = 0, 9
        WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' processes iteration ', i, '.'
    END DO
    !$omp end parallel do
END PROGRAM main