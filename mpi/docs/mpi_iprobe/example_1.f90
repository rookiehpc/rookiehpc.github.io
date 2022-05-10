!> @brief Illustrates how to probe a message in a non-blocking way.
!> @details This application is designed to probe a message at a moment at which
!> the message to receive is guaranteed not to have been arrived yet. The
!> MPI_Iprobe therefore informs that there is no message waiting and returns,
!> while an MPI_Probe would have blocked until the arrival of that message.
!> This application is meant to be run with 2 processes: a sender and a
!> receiver.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: buffer
    LOGICAL :: flag

    CALL MPI_Init(ierror)

    ! Size of the default communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    IF (my_rank .EQ. 0) THEN
        ! Wait for the other process to issue an MPI_Iprobe in vain
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        ! Send the message
        buffer = 12345
        WRITE(*,'(A,I0,A,I0,A)') 'Process ', my_rank, ': sending the message containing ', buffer, '.'
        CALL MPI_Send(buffer, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierror)
    ELSE
        ! The send has not been issued yet, this probe is vain but not blocking.
        CALL MPI_Iprobe(0, 0, MPI_COMM_WORLD, flag, MPI_STATUS_IGNORE, ierror)
        IF (.NOT. flag) THEN
            WRITE(*,'(A,I0,A)') 'Process ', my_rank, ': no message arrived yet.'
        ELSE
            ! This branching will not happen
            WRITE(*,'(A,I0,A)') 'Process ', my_rank, ': message arrived.'
        END IF

        ! Inform other MPI process that we issued the MPI_Iprobe meant to be vain
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        ! Actually receive the message
        CALL MPI_Recv(buffer, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        WRITE(*,'(A,I0,A,I0,A)') 'Process ', my_rank, ': message received containing ', buffer, '.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main