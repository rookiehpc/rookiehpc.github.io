!> @brief Illustrate how to communicate a logical between 2 MPI processes.
!> @details This application is meant to be run with 2 MPI processes: 1 sender
!> and 1 receiver. The former sends a logical to the latter, which prints it.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    LOGICAL :: boolToSend
    LOGICAL :: boolReceived

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
            ! Send the logical
            boolToSend = .TRUE.
            WRITE(*,'(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I send logical: ', boolToSend, '.'
            CALL MPI_Ssend(boolToSend, 1, MPI_LOGICAL, receiver_rank, 0, MPI_COMM_WORLD)
        CASE (receiver_rank)
            ! Receive the logical
            CALL MPI_Recv(boolReceived, 1, MPI_LOGICAL, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
            WRITE(*,'(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I received logical: ', boolReceived, '.'
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
