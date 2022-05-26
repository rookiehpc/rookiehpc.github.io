!> @brief Illustrates the use of omp_sched_auto.
!> @details A for loop is parallelised across 4 threads using the runtime
!> schedule policy. The schedule to apply is defined by omp_set_schedule as 
!> being an auto scheduling. The chunk size input is required but ignored so any
!> value is acceptable.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: i

    ! Use 4 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(4)

    ! Tell OpenMP which scheduling policy and chunk size use for runtime schedule clauses
    CALL omp_set_schedule(omp_sched_auto, 0)

    ! Parallelise the for loop using the runtime schedule
    !$omp parallel do schedule(runtime)
    DO i = 0, 9
        WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' processes iteration ', i, '.'
    END DO
    !$omp end parallel do
END PROGRAM main