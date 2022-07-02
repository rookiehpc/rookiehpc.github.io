!> @brief Exercise about sending a message in MPI.
!> @details This exercisse just consits of 2 MPI processes, the first one sends
!> a message and the second one receives it. The message to sendis just an
!> integer with the value 12345. The receiver must print the value received.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: comm_size

    ! 1) Tell MPI to start

    ! 2) Check that the application is run with 2 MPI processes
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A,I0,A)') 'This application must be run with 2 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! 3) Get my rank

    ! 4) If my rank is 0, I am the sender
        ! 4.1) Print the value to send
        ! 4.2) Send the value
    
    ! 5) If my rank is 1, I am the receiver
        ! 5.1) Receive the value
        ! 5.2) Print the value received

    ! 6) Tell MPI to stop
END PROGRAM main