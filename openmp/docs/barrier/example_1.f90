!> @brief Illustrates the OpenMP barrier synchronisation.
!> @details This application is made of a parallel region, in which two distinct
!> parts are to be executed, separated with a barrier. In each part, threads
!> have to print a message. They will print their second message only when all
!> threads will have printed the first one.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! Use 4 threads when we create a parallel region
    CALL omp_set_num_threads(4)

    ! Create the parallel region
    !$omp parallel

        ! Threads print their first message
        WRITE(*,'(A,I0,A)') '(Thread ', omp_get_thread_num(), ') I print my first message.'

        ! Make sure all threads have printed their first message before moving on.
        !$omp barrier

        ! One thread indicates that the barrier is complete.
        !$omp single
            WRITE(*,'(A)') 'The barrier is complete, which means all threads have printed their first message.'
        !$omp end single

        ! Threads print their second message
        WRITE(*,'(A,I0,A)') '(Thread ', omp_get_thread_num(), ') I print my second message.'
    !$omp end parallel
END PROGRAM main