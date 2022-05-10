!> @brief Illustrates how to send a message in a non-blocking fashion.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer
    INTEGER :: request

    CALL MPI_Init(ierror)

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            buffer = 12345
            WRITE(*,'(A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffer, '.'
            CALL MPI_Isend(buffer, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, request, ierror)
            
            ! Do other things while the MPI_Isend completes
            ! <...>

            ! Let's wait for the MPI_Isend to complete before progressing further.
            CALL MPI_Wait(request, MPI_STATUS_IGNORE, ierror)
        CASE (receiver_rank)
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,As)') 'MPI process ', my_rank, ' received value: ', buffer, '.'
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main