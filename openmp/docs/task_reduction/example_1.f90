!> @brief Illustrates how to do a task reduction.
!> @details This example consists in calculating the sum of all elements of an
!> array.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER :: total = 0
    INTEGER, PARAMETER :: ARRAY_SIZE = 10
    INTEGER, DIMENSION(ARRAY_SIZE) :: myArray
    INTEGER :: i

    ! Use 2 threads when creating OpenMP parallel regions
    CALL omp_set_num_threads(2)

    ! Initialise the array
    myArray = [(i, i=1, ARRAY_SIZE)]

    ! Calculate the sum of all elements
    !$omp parallel
        !$omp single
            !$omp taskgroup task_reduction(+: total)
                DO i = 1, ARRAY_SIZE
                    !$omp task in_reduction(+: total)
                        total = total + myArray(i)
                    !$omp end task
                END DO
            !$omp end taskgroup
        !$omp end single
    !$omp end parallel

    WRITE(*, '(A,I0,A)') 'The sum of all array elements is ', total, '.'
END PROGRAM main