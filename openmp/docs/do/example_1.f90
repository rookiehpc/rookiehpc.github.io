!> @brief Illustrates how to use the do construct.
!> @details This application contains a do loop that initialises an array. This
!> do loop is parallelised by using a do construct inside a parallel region.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: a(0:9)
    INTEGER :: i

    ! Use 2 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(2)

    ! Spawn the threads
    !$omp parallel
        ! Tell the threads to share these iterations rather than running the entire iteration set each
        !$omp do
        DO i = 0, 9
            a(i) = i
        END DO
        !$omp end do
    !$omp end parallel
END PROGRAM main