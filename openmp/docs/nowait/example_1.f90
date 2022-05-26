!> @brief Illustrates how to use a nowait clause.
!> @details A parallel region is created, in which one thread executes a single
!> construct while the other one skips it without waiting thanks to the nowait
!> clause.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! This semaphore is used to sequentialise printfs.
    LOGICAL :: handshake = .FALSE.

    ! Use 4 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(2)

    ! Create the parallel region
    !$omp parallel default(none) shared(handshake)
        !$omp single
        WRITE(*,'(A,I0,A)') 'Thread ', omp_get_thread_num(), ' got into the single construct.'
        DO WHILE (.NOT. handshake)
        END DO
        !$omp end single nowait

        !$omp critical
        IF (.NOT. handshake) THEN
            WRITE(*,'(A,I0,A,A)') 'Thread ', omp_get_thread_num(), ' skipped the single clause,', &
                                ' not waiting for the other thread thanks to the nowait clause.'
            handshake = .TRUE.
        ELSE
            WRITE(*,'(A,I0,A)') 'Thread ', omp_get_thread_num(), ' has now completed the single construct.'
        END IF
        !$omp end critical
    !$omp end parallel
END PROGRAM main