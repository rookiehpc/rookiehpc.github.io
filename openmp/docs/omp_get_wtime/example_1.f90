!> @brief Illustrates how to measure time using walltime.
!> @details This code waits for a number of seconds given, by having a loop that
!> checks how many seconds elapsed since it started, and keeps looping until
!> enough seconds have passed by.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! Time before the loop began
    DOUBLE PRECISION :: start_time
    ! Time right now
    DOUBLE PRECISION :: current_time
    ! Elapsed time since beginning of loop
    DOUBLE PRECISION :: elapsed_time
    ! Number of seconds to wait
    DOUBLE PRECISION :: to_wait = 3

    ! Initialise the start time
    start_time = omp_get_wtime()
    ! Initialise the end time
    current_time = omp_get_wtime()
    ! Initialise the elapsed time
    elapsed_time = current_time - start_time

    ! As long as fewer than 'to_wait' seconds elapsed, keep looping
    DO WHILE (elapsed_time < to_wait)
        ! Get the new time
        current_time = omp_get_wtime()
        ! Update the elapsed time
        elapsed_time = current_time - start_time
    END DO

    WRITE(*,'(F0.0,A)') elapsed_time, ' seconds have elapsed.'
END PROGRAM main