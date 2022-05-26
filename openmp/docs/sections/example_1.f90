!> @brief Illustrates how to use a sections clause.
!> @details A parallel region is created, in which a sections worksharing
!> construct is built, containing multiple section clauses defining jobs to do
!> by threads.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE
    
    ! Use 4 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(4)

    ! Create the parallel region
    !$omp parallel
        ! Create the sections
        !$omp sections
            ! Generate the first section
            !$omp section
                WRITE(*,'(A,I0,A)') 'Section 1 is executed by thread ', omp_get_thread_num(), '.'

            ! Generate the second section
            !$omp section
                WRITE(*,'(A,I0,A)') 'Section 2 is executed by thread ', omp_get_thread_num(), '.'
        !$omp end sections
    !$omp end parallel
END PROGRAM main