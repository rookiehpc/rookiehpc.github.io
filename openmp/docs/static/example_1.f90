!> @brief Illustrates the static scheduling distribution.
!> @details A for loop is parallelised across 2 threads using the static policy, in 2 situations:
!>     1) No chunksize passed
!>         - Thread 0: first half of the iterations
!>         - Thread 1: second half of the iterations
!>     2) A chunksize of 2 iterations
!>         - Thread 0 takes the first chunk, the third chunk, the fifth chunk and so on...
!>         - Thread 1 takes the second chunk, the fourth chunk, the sixth chunk and so on...
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: i

    ! Use 2 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(2)

    WRITE(*,'(A)') 'With no chunksize passed:'

    ! Parallelise the do loop using the static schedule
    !$omp parallel do schedule(static)
    DO i = 0, 9
        WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' processes iteration ', i, '.'
    END DO

    WRITE(*,'(A)') 'With a chunksize of 2:'

    ! Parallelise the do loop using the static schedule and chunks of 2 iterations
    !$omp parallel do schedule(static, 2)
    DO i = 0, 9
        WRITE(*,'(A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' processes iteration ', i, '.'
    END DO
    !$omp end parallel do
END PROGRAM main