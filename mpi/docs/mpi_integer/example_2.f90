!> @brief Illustrate how to communicate an array of ints between 2 MPI
!> processes.
!> @details This application is meant to be run with 2 MPI processes: 1 sender
!> and 1 receiver. The former sends an array of ints to the latter, which
!> prints it.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: intsToSend(0:1)
    INTEGER :: intsReceived(0:1)

    CALL MPI_Init(ierror)

    ! Check that 2 MPI processes are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*, '(A)') 'This application is meant to be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank and do the corresponding job.
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Send the ints
            intsToSend = (/12345, 67890/)
            WRITE(*,'(A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] I send ints: ', intsToSend(0), &
                                          ' and ', intsToSend(1), '.'
            CALL MPI_Ssend(intsToSend, 2, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            ! Receive the ints
            CALL MPI_Recv(intsReceived, 2, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] I received ints: ', intsReceived(0), &
                                          ' and ', intsReceived(1), '.'
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main