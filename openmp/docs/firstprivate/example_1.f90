!> @brief Illustrates the OpenMP firstprivate policy.
!> @details This example shows that when a firstprivate variable is passed to a
!> parallel region, threads work on initialised copies but that whatever
!> modification is made to their copies is not reflected onto the original
!> variable. 
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! Variable that will be firstprivate
    INTEGER :: val = 123456789

    ! Use 4 OpenMP threads
    CALL omp_set_num_threads(4)

    WRITE(*,'(A,I0,A)') 'Value of "val" before the OpenMP parallel region: ', val ,'.'

    !$omp parallel default(none) firstprivate(val)
        WRITE(*,'(A,I0,A,I0,A,I0,A)') 'Thread ', omp_get_thread_num(), ' sees "val" = ', val, &
                                      ', and updates it to be ', omp_get_thread_num(), '.'
        val = omp_get_thread_num()
    !$omp end parallel

    ! Value after the parallel region unchanged.
    WRITE(*,'(A,I0,A)') 'Value of "val" after the OpenMP parallel region: ', val, '.'
END PROGRAM main