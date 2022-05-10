!> @brief Display the error code contained in the MPI_Status.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: buffer
    INTEGER :: status(MPI_STATUS_SIZE)

    CALL MPI_Init(ierror)

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! The 'master' MPI process issues the MPI_Bsend.
            buffer = 12345
            WRITE(*,'(A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffer, '.'
            CALL MPI_Ssend(buffer, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            ! The 'slave' MPI process receives the message.
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, status, ierror)
            WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value ', buffer, &
                                          ', with error code ', status(MPI_ERROR), '.'
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main