 !> @brief Illustrates how to use the MPI_REQUEST_NULL value.
 !> @details This program is meant to be run with 2 processes: a sender and a
 !> receiver. The MPI process 0 sends an integer to MPI process 1 using a
 !> non-blocking standard send. MPI process 0 then checks that the request is
 !> set to MPI_REQUEST_NULL once it has been waited upon.
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
            CALL MPI_Isend(buffer, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, request, ierror)
            
            ! Do other things while the MPI_Isend completes
            ! <...>

            ! Let's wait for the MPI_Isend to complete before progressing further and check the request has been set to MPI_REQUEST_NULL.
            CALL MPI_Wait(request, MPI_STATUS_IGNORE, ierror)
            WRITE(*, '(A,A,A)') "[MPI process 0] The MPI_Wait completed. The request has been set to MPI_REQUEST_NULL: ", &
                merge('true ', 'false', request .EQ. MPI_REQUEST_NULL), '.'
        CASE (receiver_rank)
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main