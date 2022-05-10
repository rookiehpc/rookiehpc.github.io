!> @brief Display information contained in the MPI_Status.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer
    INTEGER :: tag
    INTEGER :: status(MPI_STATUS_SIZE)

    CALL MPI_Init(ierror)

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    SELECT CASE (my_rank)
        CASE (sender_rank)
            buffer = 12345
            tag = 67890
            WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffer, ' with tag ', tag, '.'
            CALL MPI_Ssend(buffer, 1, MPI_INTEGER, receiver_rank, tag, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierror)
            WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value ', buffer, &
                                     ' from rank ', status(MPI_SOURCE), ', with tag ', status(MPI_TAG), &
                                     ' and error code ', status(MPI_ERROR), '.'
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main