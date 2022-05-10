!> @brief Illustrate how to communicate a string between 2 MPI processes.
!> @details This application is meant to be run with 2 MPI processes: 1 sender
!> and 1 receiver. The former sends a string to the latter, which prints it.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    CHARACTER(LEN=11) :: stringToSend
    CHARACTER(LEN=11) :: stringReceived

    CALL MPI_Init(ierror)

    ! Check that 2 MPI processes are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank and do the corresponding job.
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Sends the string
            stringToSend = 'Hello world'
            WRITE(*,'(A,I0,A,A,A)') '[MPI process ', my_rank, '] I send string: "', stringToSend, '".'
            CALL MPI_Ssend(stringToSend, 11, MPI_CHARACTER, receiver_rank, 0, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            ! Receives the string
            CALL MPI_Recv(stringReceived, 11, MPI_CHARACTER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,A,A)') '[MPI process ', my_rank, '] I received string: "', stringReceived, '".'
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main