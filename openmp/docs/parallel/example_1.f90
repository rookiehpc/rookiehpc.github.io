!> @brief Illustrates the OpenMP parallel region creation.
!> @details Two parallel regions are created:
!>     - 1st one: using the number of threads indicated by the OMP_NUM_THREADS environment variable
!>     - 2nd step: using the number of threads overriden by omp_set_num_threads
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    !$omp parallel
        !$omp single
            WRITE(*,'(A,I0,A)') 'From the environment variable, parallel regions contain ', omp_get_num_threads(), ' threads.'
        !$omp end single
    !$omp end parallel


    CALL omp_set_num_threads(7)
    !$omp parallel
        !$omp single
            WRITE(*,'(A,I0,A)') 'After overwriting, parallel regions contain ', omp_get_num_threads(), ' threads.'
        !$omp end single
    !$omp end parallel
END PROGRAM main