!> @brief Illustrates how to use a single clause.
!> @details A parallel region is created, in which a certain part is executed by
!> every thread, and another part is executed only by a single thread.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! Use 4 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(4)

    ! Create the parallel region
    !$omp parallel
        WRITE(*,'(A,I0,A)') '(Thread ', omp_get_thread_num(), ') Every thread executes this printf.'

        !$omp barrier

        !$omp single
            WRITE(*,'(A,I0,A,A)') '(Thread ', omp_get_thread_num(), ') Only a single thread executes this printf, ', &
                                'I happen to be the one picked.'
        !$omp end single
    !$omp end parallel
END PROGRAM main