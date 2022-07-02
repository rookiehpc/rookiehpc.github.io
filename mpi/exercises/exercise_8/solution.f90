!> @brief Solution to the 'Ordered hello world' MPI exercise.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER :: i

    CALL MPI_Init(ierror)

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    i = 0

    ! I wait for everybody before me
    DO WHILE (i .LT. my_rank)
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)
        i = i + 1
    END DO

    ! I print my message
    WRITE(*, '(A,I0,A)') '[MPI Process ', my_rank, '] Hello World!'

    ! I wait for everybody after me
    DO WHILE (i .LT. comm_size)
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)
        i = i + 1
    END DO

    CALL MPI_Finalize(ierror)
END PROGRAM main