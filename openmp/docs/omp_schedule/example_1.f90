!> @brief Illustrates how to tell OpenMP to apply the schedule specified from
!> OMP_SCHEDULE.
!> @details The runtime schedule is specified so that OpenMP fetches the
!> scheduling policy to apply from the environment variable OMP_SCHEDULE. In
!> this example, we assume that OMP_SCHEDULE has been set to 'static,4'.
PROGRAM MAIN
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: i

    ! Use 2 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(2)

    ! Parallelise the for loop using the runtime schedule, thus applying what OMP_SCHEDULE says
    !$omp parallel do schedule(runtime)
    DO i = 0, 9
        WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' processes iteration ', i, '.'
    END DO
    !$omp end parallel do
END PROGRAM main