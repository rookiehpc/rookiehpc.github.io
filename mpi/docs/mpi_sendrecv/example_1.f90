!> @brief Pair communications between 2 MPI processes sending a message to each other.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: buffer_send
    INTEGER :: buffer_recv
    INTEGER :: peer
    INTEGER :: tag_send
    INTEGER :: tag_recv

    CALL MPI_Init(ierror)

    ! Make sure exactly 2 MPI processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 MPI processes'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Prepare parameters
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    buffer_send = MERGE(12345, 67890, my_rank .EQ. 0)
    tag_send = 0
    tag_recv = tag_send
    peer = MERGE(1, 0, my_rank == 0)

    ! Issue the send + receive at the same time
    WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank ,' sends value ', buffer_send ,' to MPI process ', peer ,'.'
    CALL MPI_Sendrecv(buffer_send, 1, MPI_INTEGER, peer, tag_send, &
                      buffer_recv, 1, MPI_INTEGER, peer, tag_recv, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
    WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank ,' received value ', buffer_recv ,' from MPI process ', peer ,'.'

    CALL MPI_Finalize(ierror)
END PROGRAM main