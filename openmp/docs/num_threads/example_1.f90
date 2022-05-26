!> @brief Illustrates how to use the num_threads clause.
!> @details This code generates three parallel regions. The number of threads to
!> use for all parallel regions has been set to 4 by the omp_set_num_threads
!> function. This value will be overwritten for the second parallel region only,
!> using the num_threads clause.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! Tell OpenMP to use 4 threads in parallel regions from now on
    CALL omp_set_num_threads(4)

    ! Create a first parallel region, which therefore contains 4 threads
    !$omp parallel
        ! Each thread prints its identifier
        WRITE(*,'(A,I0,A,I0,A)') 'Loop 1: We are ', omp_get_num_threads(), ' threads, I am thread ', omp_get_thread_num(), '.'
    !$omp end parallel

    ! Create a second parallel region, overwriting the number of threads to use with num_threads
    !$omp parallel num_threads(3)
        ! Each thread prints its identifier
        WRITE(*,'(A,I0,A,I0,A)') 'Loop 2: We are ', omp_get_num_threads(), ' threads, I am thread ', omp_get_thread_num(), '.'
    !$omp end parallel

    ! Create a third parallel region, no overwriting from num_threads, so back to 4 threads again
    !$omp parallel
        ! Each thread prints its identifier
        WRITE(*,'(A,I0,A,I0,A)') 'Loop 3: We are ', omp_get_num_threads(), ' threads, I am thread ', omp_get_thread_num(), '.'
    !$omp end parallel
END PROGRAM main