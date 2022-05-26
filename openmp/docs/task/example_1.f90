!> @brief Illustrates how to create tasks.
!> @details This application consists of a thread, in an OpenMP parallel
!> region, that spawns tasks.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! Use 3 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(3)

    ! Create the parallel region
    !$omp parallel
        ! One thread will spawn the tasks
        !$omp single
            ! Spawn the first task
            !$omp task
                WRITE(*,'(A,I0,A)') 'Task 1 executed by thread ', omp_get_thread_num(), '.'
            !$omp end task

            ! Spawn the second task
            !$omp task
                WRITE(*,'(A,I0,A)') 'Task 2 executed by thread ', omp_get_thread_num(), '.'
            !$omp end task

            ! Wait for both tasks to finish
            !$omp taskwait
        !$omp end single
    !$omp end parallel
END PROGRAM main