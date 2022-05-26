!> @brief Illustrates the OpenMP linear policy.
!> @details This example shows that when a linear variable is passed to a
!> parallelised for loop, the value of that variable is the original value plus 
!> the iteration logical number times the linear-step. After the OpenMP parallel
!> for, the value of the original variable is that of the linear variable at the
!> last iteration.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: i
    ! Variable that will be private
    INTEGER :: val = 1

    ! Use 4 OpenMP threads
    CALL omp_set_num_threads(4)

    WRITE(*,'(A,I0,A)') 'Value of "val" before the OpenMP parallel for is ', val, '.'

    !$omp parallel default(none) firstprivate(val)
        !$omp do linear(val:2)
        DO i = 0, 9
            WRITE(*,'(A,I0,A,I0,A,I0,A)') 'Thread ', omp_get_thread_num() ,' sees "val" = ', val ,' at iteration ', i ,'.'
        END DO
        !$omp end do
    !$omp end parallel

    WRITE(*,'(A,I0,A)') 'Value of "val" after the OpenMP parallel for is ', val ,'.'
END PROGRAM main