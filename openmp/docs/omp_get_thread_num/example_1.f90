!> @brief Illustrates how to get a thread identifier.
!> @details This code generates a parallel region, in which threads print their
!> identifier.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! Tell OpenMP to use 4 threads in parallel regions
    CALL omp_set_num_threads(4)

    ! Create the OpenMP parallel region, which will contain 4 threads
    !$omp parallel
        ! Each thread prints its identifier
        WRITE(*,'(A,I0,A)') 'I am thread ', omp_get_thread_num(), '.'
    !$omp end parallel
END PROGRAM main