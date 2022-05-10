!> @brief Illustrates how to issue a message as soon as possible in a
!> non-blocking fashion.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver.
!>
!> (Note to readers: the use of an MPI barrier is to ensure that the MPI_Irsend 
!> is issued after the corresponding MPI receive is issued.)
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer_sent
    INTEGER :: received
    TYPE(MPI_Request) :: request

    CALL MPI_Init()

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            CALL MPI_Barrier(MPI_COMM_WORLD)

            buffer_sent = 12345
            WRITE(*,'(A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffer_sent, '.'
            CALL MPI_Irsend(buffer_sent, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, request)

            ! Do something else while the MPI_Irsend progresses
            ! <...>

            ! Wait for the underlying MPI_Rsend to complete.
            CALL MPI_Wait(request, MPI_STATUS_IGNORE)
        CASE (receiver_rank)
            CALL MPI_Irecv(received, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, request)

            ! Tell the other process that the receive is posted, so the ready send can be issued
            CALL MPI_Barrier(MPI_COMM_WORLD)

            ! Wait for the underlying MPI_Recv to complete.
            CALL MPI_Wait(request, MPI_STATUS_IGNORE)
            WRITE(*,'(A,I0,A,I0,A)') 'MPI process ', my_rank, ' receives value ', received, '.'
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
