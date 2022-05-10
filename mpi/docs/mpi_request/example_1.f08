!> @brief Illustrates how to use an MPI_Request to wait for the completion of a
!> non-blocking operation.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer
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
            buffer = 12345
            WRITE(*,'(A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends the value ', buffer, '.'
            CALL MPI_Ssend(buffer, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD)
        CASE (receiver_rank)
            CALL MPI_Irecv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, request)

            ! Do some other things while the underlying MPI_Recv progresses.
            WRITE(*,'(A,I0,A)') 'MPI process ', my_rank, ' issued the MPI_Irecv and obtained an MPI_Request.'

            ! Wait for the MPI_Recv to complete.
            WRITE(*,'(A,I0,A)') 'MPI process %d waits on that MPI_Request.'
            CALL MPI_Wait(request, MPI_STATUS_IGNORE)
            WRITE(*,'(A,A,I0,A)') 'The MPI_Wait completed, which means the request (pointing to an MPI_Recv) is complete too', &
                                ' (value received: ', buffer ,').'
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
