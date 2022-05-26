!> @brief Illustrates how to use an atomic construct.
!> @details This application consists of 4 threads incrementing a shared
!> variable. This situation is a classic data-race configuration, but the atomic
!> construct guarantees correctness by making sure the variable is accessed
!> atomically. In a nutshell, altough we still do not know in which order the
!> increments are done, we are certain they do not corrupt each other. This
!> situation is to illustrate the atomic construct, in real-world codes, you
!> will want to use a reduction clause instead.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: total = 0
    INTEGER :: i

    ! Use 4 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(4)

    ! Create the parallel region
    !$omp parallel default(none) shared(total)
        DO i = 0, 9
            ! Atomically add one to the total
            !$omp atomic
            total = total + 1
        END DO
    !$omp end parallel

    WRITE(*,'(A,I0,A)') 'Total = ', total, '.'
END PROGRAM main