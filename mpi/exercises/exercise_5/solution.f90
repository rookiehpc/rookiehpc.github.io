!> @brief Solution to the distributed sum MPI exercise.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank
    INTEGER :: my_value
    INTEGER :: total

    CALL MPI_Init(ierror)

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Initialise my reduction variable
    my_value = my_rank * 100
    WRITE(*, '(A,I0,A,I0,A)') 'Value held by MPI process ', my_rank ,': ', my_value ,'.'

    ! Perform the reduction
    CALL MPI_Reduce(my_value, total, 1, MPI_INTEGER , MPI_SUM, 0, MPI_COMM_WORLD, ierror)

    ! If I am process 0, I print the result
    IF (my_rank == 0) THEN
        WRITE(*, '(A,I0,A,I0,A)') 'Total sum reduced at MPI process ', my_rank ,': ', total ,'.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main