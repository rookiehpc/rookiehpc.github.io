!> @brief Illustrates how to get the number of threads.
!> @details This code prints the number of threads at two specific locations:
!>     1) Outside an OpenMP parallel region
!>     2) Inside an OpenMP parallel region
PROGRAM main
    USE omp_lib

    ! Tell OpenMP to use 4 threads in parallel regions
    CALL omp_set_num_threads(4)

    ! 1) Outside the OpenMP parallel region
    WRITE(*,'(A,I0,A)') 'Outside the OpenMP parallel region, we are ', omp_get_num_threads(), ' threads.'

    ! Create the OpenMP parallel region, which will contain 4 threads
    !$omp parallel
        ! 2) Inside the OpenMP parallel region
        WRITE(*,'(A,I0,A)') 'Inside the OpenMP parallel region, we are ', omp_get_num_threads(), ' threads.'
    !$omp end parallel
END PROGRAM main