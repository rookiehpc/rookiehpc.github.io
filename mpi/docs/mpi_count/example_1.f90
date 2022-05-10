!> @brief Illustrate how to use the MPI_Count datatype.
!> @details This application consists of 2 MPI processes. MPI process 0 creates
!> a count, and sends it to MPI process 1, which then prints it.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER :: my_type
    INTEGER(KIND=MPI_COUNT_KIND) :: count

    CALL MPI_Init(ierror)

    ! Check that 2 MPI processes are spawn
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 2 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    IF (my_rank .EQ. 0) THEN
        ! Create the MPI datatype
        CALL MPI_Type_contiguous(10, MPI_INTEGER, my_type, ierror)
        CALL MPI_Type_commit(my_type, ierror)

        ! Retrieve the size of the MPI datatype created
        CALL MPI_Type_size_x(my_type, count, ierror)
        CALL MPI_Send(count, 1, MPI_COUNT, 1, 0, MPI_COMM_WORLD, ierror)
    ELSE
        CALL MPI_Recv(count, 1, MPI_COUNT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        WRITE(*, '(A,I0,A)') 'The type created would generate a message of ', count, ' bytes.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main