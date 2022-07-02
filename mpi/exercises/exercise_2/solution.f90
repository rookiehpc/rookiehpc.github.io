!> @brief Solution to the simple send-receive in MPI.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER :: value

    ! 1) Tell MPI to start
    CALL MPI_Init(ierror)

    ! 2) Check that the application is run with 2 MPI processes
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A,I0,A)') 'This application must be run with 2 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! 3) Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! 4) If my rank is 0, I am the sender
    IF (my_rank .EQ. 0) THEN
        ! 4.1) Print the value to send
        value = 12345
        WRITE(*, '(A,I0,A,I0,A)') 'I am process ', my_rank, ' and I send value ', value, '.'

        ! 4.2) Send the value
        CALL MPI_Send(value, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierror)    
    ! 5) If my rank is 1, I am the receiver
    ELSE
        ! 5.1) Receive the value
        CALL MPI_Recv(value, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)

        ! 5.2) Print the value received
        WRITE(*, '(A,I0,A,I0,A)') 'I am process ', my_rank, ' and I received value ', value, '.'
    END IF

    ! 6) Tell MPI to stop
    CALL MPI_Finalize(ierror)
END PROGRAM main