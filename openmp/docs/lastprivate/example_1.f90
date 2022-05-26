!> @brief Illustrates the OpenMP lastprivate policy.
!> @details This example shows that when a lastprivate variable is passed to a
!> parallelised for loop, threads work on uninitialised copies but, at the end
!> of the parallelised for loop, the thread in charge of the last iteration
!> sets the value of the original variable to that of its own copy.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! Variable that will be lastprivate
    INTEGER :: val = 123456789
    INTEGER :: i

    ! Use 4 OpenMP threads
    CALL omp_set_num_threads(4)

    WRITE(*,'(A,I0,A)') 'Value of "val" before the OpenMP parallel region: ', val ,'.'

    !$omp parallel
        !$omp do lastprivate(val)
        DO i = 0, omp_get_thread_num()-1
            val = omp_get_thread_num()
        END DO
        !$omp end do
    !$omp end parallel

    ! Value after the parallel region unchanged.
    WRITE(*,'(A,I0,A,A,I0,A)') 'Value of "val" after the OpenMP parallel region: ', val ,'.', &
                             ' Thread ', val ,' was therefore the last one to modify it.'
END PROGRAM main