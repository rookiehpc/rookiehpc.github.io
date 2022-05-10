!> @brief Display the tag contained in the MPI_Status.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer
    INTEGER :: tag
    INTEGER :: status(MPI_STATUS_SIZE)

    CALL MPI_Init(ierror)

    ! Get the size of the communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    SELECT CASE (my_rank)
        CASE (sender_rank)
            buffer = 12345
            tag = 67890
            WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffer, ', with tag ', tag, '.'
            CALL MPI_Ssend(buffer, 1, MPI_INTEGER, receiver_rank, tag, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierror)
            WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value ', buffer, ', with tag ', status(MPI_TAG), '.'
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main