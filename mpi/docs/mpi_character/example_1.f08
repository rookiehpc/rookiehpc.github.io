!> @brief Illustrate how to communicate a character between 2 MPI processes.
!> @details This application is meant to be run with 2 MPI processes: 1 sender
!> and 1 receiver. The former sends a character to the latter, which prints it.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    CHARACTER :: characterToSend
    CHARACTER :: characterReceived

    CALL MPI_Init()

    ! Check that 2 MPI processes are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job.
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Sends the character
            characterToSend = 'A'
            WRITE(*,'(A,I0,A,A,A)') '[MPI process ', my_rank, '] I send character: "', characterToSend, '".'
            CALL MPI_Ssend(characterToSend, 1, MPI_CHARACTER, receiver_rank, 0, MPI_COMM_WORLD)
        CASE (receiver_rank)
            ! Receives the character
            CALL MPI_Recv(characterReceived, 1, MPI_CHARACTER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
            WRITE(*,'(A,I0,A,A,A)') '[MPI process ', my_rank, '] I received character: "', characterReceived, '".'
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
