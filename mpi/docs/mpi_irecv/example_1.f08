!> @brief Illustrates how to receive a message in a non-blocking fashion.
!> @details This application is meant to be run with 2 processes: 1 sender and 1
!> receiver. The receiver immediately issues the MPI_Irecv, then it moves on
!> printing a message while the reception takes place in the meantime. Finally,
!> the receiver waits for the underlying MPI_Recv to print the value received.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer
    INTEGER :: received
    TYPE(MPI_Request) :: request

    CALL MPI_Init()

    ! Get the number of processes
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! The 'master' MPI process sends the message.
            buffer = 12345
            WRITE(*,'(A,I0,A,I0,A)') '[Process ', my_rank, '] I send the value ', buffer, '.'
            CALL MPI_Ssend(buffer, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD)
        CASE (receiver_rank)
            ! The 'slave' MPI process receives the message.
            WRITE(*,'(A,I0,A)') '[Process ', my_rank, '] I issue the MPI_Irecv to receive the message as a background task.'
            CALL MPI_Irecv(received, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, request)

            ! Do other things while the MPI_Irecv completes.
            WRITE(*,'(A,I0,A)') '[Process ', my_rank, '] The MPI_Irecv is issued, I now moved on to print this message.'

            ! Wait for the MPI_Recv to complete.
            CALL MPI_Wait(request, MPI_STATUS_IGNORE)
            WRITE(*,'(A,I0,A,A,I0,A)') &
                '[Process ', my_rank, '] The MPI_Irecv completed, therefore so does the underlying MPI_Recv.', &
                ' I received the value ', received, '.'
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
