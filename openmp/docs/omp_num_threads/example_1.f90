!> @brief Illustrates the effect of the environment variable OMP_NUM_THREADS.
!> @details This application assumes that the environment variable
!> OMP_NUM_THREADS has been set to a specific number. For instance, to put the
!> value 4 in the environment variable OMP_NUM_THREADS:
!> - For Windows: SET OMP_NUM_THREADS=4
!> - For Linux  Mac: export OMP_NUM_THREADS=4
!> This application is meant to be executed after the environment variable
!> OMP_NUM_THREADS was updated. It creates a parallel region, and prints the
!> number of threads in that parallel region, which is that of OMP_NUM_THREADS.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    !$omp parallel
        ! Each thread prints its identifier
        WRITE(*,'(A,I0,A,I0,A)') 'We are ', omp_get_num_threads(), ' threads, I am thread ', omp_get_thread_num(), '.'
    !$omp end parallel
END PROGRAM main