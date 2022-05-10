!> @brief Illustrates how to broadcast a message.
!> @details This code picks a process as the broadcast root, and makes it
!> broadcast a specific value. Other processes participate to the broadcast as
!> receivers. These processes then print the value they received via the 
!> broadcast.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: my_rank
    ! Determine the rank of the broadcast emitter process
    INTEGER, PARAMETER :: broadcast_root = 0
    INTEGER :: buffer

    CALL MPI_Init()

    ! Get my rank in the communicator
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    IF (my_rank .EQ. broadcast_root) THEN
        buffer = 12345
        WRITE(*,'(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I am the broadcast root, and send value ', buffer, '.'
    END IF
    CALL MPI_Bcast(buffer, 1, MPI_INTEGER, broadcast_root, MPI_COMM_WORLD)
    IF (my_rank .NE. broadcast_root) THEN
        WRITE(*,'(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I am a broadcast receiver, and obtained value ', buffer, '.'
    END IF

    CALL MPI_Finalize()
END PROGRAM main
