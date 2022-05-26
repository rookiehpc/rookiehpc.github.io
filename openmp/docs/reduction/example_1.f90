!> @brief Illustrates how to do a classic reduction.
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
    !$omp parallel do default(none) shared(myArray) reduction(+: total)
    DO i = 1, ARRAY_SIZE
        total = total + myArray(i)
    END DO
    !$omp end parallel do

    WRITE(*, '(A,I0,A)') 'The sum of all array elements is equal to ', total, '.'
END PROGRAM main