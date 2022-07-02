PROGRAM main
    IMPLICIT NONE

    INTEGER, PARAMETER :: ARRAY_SIZE = 10
    INTEGER :: a(0:ARRAY_SIZE-1)
    INTEGER :: i

    ! 1) Create the OpenMP parallel region
    !$OMP parallel default(none) shared(a,i)
        ! 1.1) Create the for construct and initialise the array elements
        !$OMP do
        DO i = 0, ARRAY_SIZE-1
            a(i) = i * i
        END DO
    !$OMP END parallel

    ! 2) Print the array elements, sequentially
    DO i = 0, ARRAY_SIZE-1
        WRITE(*, '(A,I0,A,I2)') 'a(', i, ') = ', a(i)
    END DO
END PROGRAM main