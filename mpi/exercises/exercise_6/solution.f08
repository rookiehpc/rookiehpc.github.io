!> @brief Solution to the MPI exercise: 'Switch to non-blocking'.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER :: value
    TYPE(MPI_Request) :: request

    CALL MPI_Init()

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A)') 'This application must be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    IF (my_rank == 0) THEN
        value = 12345

        ! Launch the non-blocking send
        CALL MPI_Isend(value, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, request)
        WRITE(*, '(A,I0,A)') '[MPI process ', my_rank ,'] I launched the non-blocking send.'

        ! Wait for the non-blocking send to complete
        CALL MPI_Wait(request, MPI_STATUS_IGNORE)
        WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank, '] The wait completed, so I sent value ', value, '.'
    ELSE
        value = 0

        ! Launch the non-blocking receive
        CALL MPI_Irecv(value, 1, MPI_INTEGER, 0, MPI_ANY_TAG, MPI_COMM_WORLD, request)
        WRITE(*, '(A,I0,A)') '[MPI process ', my_rank ,'] I launched the non-blocking receive.'

        ! Wait for the non-blocking send to complete
        CALL MPI_Wait(request, MPI_STATUS_IGNORE)
        WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank, '] The wait completed, so I received value ', value, '.'
    END IF

    CALL MPI_Finalize()
END PROGRAM main