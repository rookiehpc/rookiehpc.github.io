!> @brief Solution to the MPI exercise 'Odd-even scatter'.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER, PARAMETER :: ROWS = 4
    INTEGER, PARAMETER :: COLS = 3
    INTEGER, PARAMETER :: count = (ROWS * COLS) / 2
    INTEGER :: entire_array(0:ROWS-1, 0:COLS-1)
    INTEGER :: my_array(0:count-1)
    INTEGER :: i
    INTEGER :: j
    INTEGER :: BRICK
    INTEGER :: BRICK_RESIZED
    INTEGER(KIND=MPI_ADDRESS_KIND) :: lb = 0
    INTEGER :: counts(3) = (/0, 1, 1/)
    INTEGER :: displacements(3) = (/0, 1, 0/)

    CALL MPI_Init(ierror)

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 3) THEN
        WRITE(*,'(A,I0,A)') 'This application must be run with 3 processes, not ', comm_size ,'.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    IF (my_rank .EQ. 0) THEN
        ! Initialise the array
        DO j = 0, COLS - 1
            DO i = 0, ROWS - 1
                entire_array(i,j) = j * ROWS + i
                WRITE(*, '(A,I2)', advance = "no") '  ', entire_array(i,j)
            END DO
            WRITE(*, '(A)') ' '
        END DO
        WRITE(*, '(A)') ' '

        ! Create a vector extracting an integer every two integers
        CALL MPI_Type_vector(ROWS * COLS / 2, 1, 2, MPI_INTEGER, BRICK, ierror)

        ! Resize the vector so that we can offset them only by the size of an integer
        CALL MPI_Type_create_resized(BRICK, lb, SIZEOF(i), BRICK_RESIZED, ierror)
        CALL MPI_Type_commit(BRICK_RESIZED, ierror)

        ! Participate to the scatterv, receiving nothing though
        CALL MPI_Scatterv(entire_array, counts, displacements, BRICK_RESIZED, my_array, 0, MPI_INTEGER, &
            0, MPI_COMM_WORLD, ierror)
    ELSE
        ! Participate to the scatterv, sending nothing though
        CALL MPI_Scatterv(entire_array, counts, displacements, BRICK_RESIZED, my_array, count, MPI_INTEGER, &
            0, MPI_COMM_WORLD, ierror)

        ! Print what has been received
        WRITE(*, '(A,I0,A)', advance = "no") 'Received on MPI process ', my_rank, ':'
        DO i = 0, count - 1
            WRITE(*, '(A,I2)', advance = "no") '  ', my_array(i)
        END DO
        WRITE(*, '(A)') ' '
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main