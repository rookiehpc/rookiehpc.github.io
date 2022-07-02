!> @brief Exercise to replace blocking with non-blocking communications.
!> @details This application consists of 2 MPI processes, a sender and a
!> receiver. They exchange an integer using a blocking send and a blocking
!> receive. The aim is to replace these with their non-blocking counterparts and
!> handle the non-blocking communication.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER :: value

    CALL MPI_Init()

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A)') 'This application must be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    IF (my_rank == 0) THEN
        value = 12345
        WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I sent value ', value, '.'
        CALL MPI_Send(value, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD)
    ELSE
        value = 0
        CALL MPI_Recv(value, 1, MPI_INTEGER, 0, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
        WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I received value ', value, '.'
    END IF

    CALL MPI_Finalize()
END PROGRAM main