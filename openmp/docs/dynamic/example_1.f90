!> @brief Illustrates the dynamic scheduling distribution.
!> @details A for loop is parallelised across 2 threads using the dynamic
!> policy, in 2 situations:
!>     1) No chunksize passed
!>     2) A chunksize of 2 iterations
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: i

    ! Use 2 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(2)

    WRITE(*,'(A)') 'With no chunksize passed:'

    ! Parallelise the for loop using the dynamic schedule
    !$omp parallel
        !$omp do schedule(dynamic)
        DO i = 0, 9
            WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' processes iteration ', i, '.'
        END DO
        !$omp end do
    !$omp end parallel

    WRITE(*,'(A)') 'With a chunksize of 2:'

    ! Parallelise the for loop using the dynamic schedule and chunks of 2 iterations
    !$omp parallel
        !$omp do schedule(dynamic,2)
        DO i = 0, 9
            WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' processes iteration ', i, '.'
        END DO
        !$omp end do
    !$omp end parallel
END PROGRAM main