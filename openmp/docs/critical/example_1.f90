!> @brief Illustrates how to use a critical clause.
!> @details This application consists of thread calculating a factorial number.
!> These threads then sum their value to a common total result. Each thread
!> prints the value of the total before and after they add their own value, 
!> which is encapsulated in a critical block to avoid data races.
PROGRAM main
    USE omp_lib

    IMPLICIT none

    INTEGER :: total = 0
    INTEGER :: my_value
    INTEGER :: i

    ! Use 4 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(4)

    ! Create the parallel region
    !$omp parallel default(none) shared(total) private(my_value)
        ! Calculate my factorial
        my_value = 1
        DO i = 2, omp_get_thread_num()
            my_value = my_value * i
        END DO

        ! Add my value to the total
        !$omp critical
            WRITE(*,'(A,I0,A,I0,A,I0,A)') '(Thread ', omp_get_thread_num(), ') Total before I add my value (', &
                                          my_value, '): ', total, '.'
            total = total + my_value
            WRITE(*,'(A,I0,A,I0,A)') '(Thread ', omp_get_thread_num(), ') Total after I added my value: ', total, '.'
        !$omp end critical
    !$omp end parallel
END PROGRAM main