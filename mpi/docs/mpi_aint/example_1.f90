!> @brief Illustrate how to use the MPI_AINT MPI_Datatype.
!> @details This application consists of 2 MPI processes. The MPI process 0
!> sends the address takes the address of its local variable to the MPI process
!> 1, which prints it.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER :: my_variable
    INTEGER(KIND=MPI_ADDRESS_KIND) :: my_variable_address
    INTEGER(KIND=MPI_ADDRESS_KIND) :: remote_variable_address

    CALL MPI_Init(ierror)

    ! Check that only 2 MPI Processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 2 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    IF (my_rank .EQ. 0) THEN
            ! Declare a local variable, get its address and send it to MPI Process 0
            CALL MPI_Get_address(my_variable, my_variable_address, ierror)
            WRITE(*, '(A,I0,A,I0,A)') '[MPI Procees ', my_rank, '] The address of my local variable is ', my_variable_address ,'.'
            CALL MPI_Send(my_variable_address, 1, MPI_AINT, 1, 0, MPI_COMM_WORLD, ierror)
    ELSE
        ! Get the address of the local variable held on MPI process 1 and print it
        CALL MPI_Recv(remote_variable_address, 1, MPI_AINT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        WRITE(*, '(A,I0,A,I0,A)') '[MPI Procees ', my_rank, '] The address of the remote variable is ', remote_variable_address ,'.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main