!> @brief Illustrates how to use the omp_set_num_threads function.
!> @details This code generates two parallel regions. The first one will use the
!> number of threads contained in the environment variable OMP_NUM_THREADS. The
!> code then overwrites that value for all upcoming parallel regions by calling
!> the omp_set_num_threads function and using 1 more thread than what was used
!> for the first parallel region. Finally, it creates a second parallel region
!> to show the difference in the number of threads.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: current_num_threads = 0

    ! Create the OpenMP parallel region, containing the number of threads as defined by OMP_NUM_THREADS
    !$omp parallel
        ! Each thread prints its identifier
        WRITE(*,'(A,I0,A,I0,A)') 'Loop 1: We are ', omp_get_num_threads(), ' threads, I am thread ', omp_get_thread_num(), '.'

        !$omp single
            current_num_threads = omp_get_num_threads()
        !$omp end single
    !$omp end parallel

    ! Tell OpenMP to now use one more thread in parallel regions
    CALL omp_set_num_threads(current_num_threads+1)

    ! Create the OpenMP parallel region, which will contain 8 threads
    !$omp parallel
        ! Each thread prints its identifier
        WRITE(*,'(A,I0,A,I0,A)') 'Loop 2: We are ', omp_get_num_threads(), ' threads, I am thread ', omp_get_thread_num(), '.'
    !$omp end parallel
END PROGRAM main