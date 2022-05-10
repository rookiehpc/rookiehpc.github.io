!> @brief Illustrates how to receive a message using persistent communications.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer
    INTEGER :: i
    INTEGER :: request

    CALL MPI_Init(ierror)

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*, '(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! The 'master' MPI process issues the MPI_Ssend.
            DO i = 0, 2
                buffer = 12345 + i
                WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffer, ' for message ', i, '.'
                CALL MPI_Ssend(buffer, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, ierror)
            END DO
        CASE (receiver_rank)
            ! The 'slave' MPI process receives the message.
            ! Fill a request handle describing the arguments to pass for reception
            CALL MPI_Recv_init(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, request, ierror)
            DO i = 0, 2
                ! Launch the reception
                CALL MPI_Start(request, ierror)
                ! Wait for the reception to complete
                CALL MPI_Wait(request, MPI_STATUS_IGNORE, ierror)
                WRITE(*, '(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value ', buffer, ' for message ', i, '.'
            END DO
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main
