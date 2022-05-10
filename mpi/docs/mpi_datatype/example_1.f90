!> @brief Use a variable to tell what MPI_Datatype to use.
PROGRAM main
	USE mpi

	IMPLICIT NONE

	INTEGER :: ierror
	INTEGER :: msg_type
	INTEGER :: my_rank
	INTEGER :: msg

    CALL MPI_Init(ierror)

    msg_type = MPI_INTEGER
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    IF (my_rank .EQ. 0) THEN
        msg = 12345
        WRITE(*,'(A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', msg, '.'
        CALL MPI_Ssend(msg, 1, msg_type, 1, 0, MPI_COMM_WORLD, ierror)
    ELSE
        CALL MPI_Recv(msg, 1, msg_type, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        WRITE(*,'(A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value ', msg, '.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main