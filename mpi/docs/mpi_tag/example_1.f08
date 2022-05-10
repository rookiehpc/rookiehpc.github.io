!> @brief Display the tag contained in the MPI_Status.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer
    INTEGER :: tag
    TYPE(MPI_Status) :: status

    CALL MPI_Init()

    ! Get the size of the communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    SELECT CASE (my_rank)
        CASE (sender_rank)
            buffer = 12345
            tag = 67890
            WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffer, ', with tag ', tag, '.'
            CALL MPI_Ssend(buffer, 1, MPI_INTEGER, receiver_rank, tag, MPI_COMM_WORLD)
        CASE (receiver_rank)
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, MPI_ANY_TAG, MPI_COMM_WORLD, status)
            WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value ', buffer, ', with tag ', status % MPI_TAG, '.'
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
