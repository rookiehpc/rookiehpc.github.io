!> @brief Demonstrates how to tell MPI to not fill the MPI_Status.
!> @details This application is designed around a simple message send. There are
!> two MPI processes: a sender and a receiver. The receiver will issue an
!> MPI_Recv and tell MPI not to fill an MPI_Status.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer

    CALL MPI_Init(ierror)

    ! Check that only 2 MPI processes are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            buffer = 12345
            CALL MPI_Ssend(buffer, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value: ', buffer, '.'
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main