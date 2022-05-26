!> @brief Illustrates the use of the omp_sched_kind.
!> @details A do loop is parallelised across 4 threads using the runtime
!> schedule policy. The schedule to apply is defined by omp_set_schedule as 
!> being a dynamic scheduling with chunks made of 4 iterations.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER(KIND=omp_sched_kind) :: kind
    INTEGER :: i

    ! Use 4 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(4)

    ! Tell OpenMP which scheduling policy and chunk size use do runtime schedule clauses
    kind = omp_sched_dynamic
    CALL omp_set_schedule(kind, 2)

    ! Parallelise the do loop using the runtime schedule
    !$omp parallel do schedule(runtime)
    DO i = 0, 9
        WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' processes iteration ', i, '.'
    END DO
    !$omp end parallel do
END PROGRAM main